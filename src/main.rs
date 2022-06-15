#![deny(warnings)]
#![allow(dead_code)]

#![feature(default_alloc_error_handler)]
#![feature(extern_types)]
#![feature(lang_items)]
#![feature(start)]
#![windows_subsystem="console"]
#![no_std]

#[cfg(windows)]
#[link(name="msvcrt")]
extern { }

extern crate alloc;

use composable_allocators::{AsGlobal, System};

#[global_allocator]
static ALLOCATOR: AsGlobal<System> = AsGlobal(System);

#[cfg(not(feature="debug"))]
#[panic_handler]
fn panic(_panic: &core::panic::PanicInfo) -> ! {
    exit_no_std::exit(b'P')
}

#[cfg(all(windows, target_env="gnu", not(feature="debug")))]
#[no_mangle]
extern fn rust_eh_register_frames () { }

#[cfg(all(windows, target_env="gnu", not(feature="debug")))]
#[no_mangle]
extern fn rust_eh_unregister_frames () { }

mod world;
use world::*;

use alloc::boxed::Box;
use components_arena::{Arena, Component, Id};
use core::any::Any;
use core::cmp::{max, min};
use core::num::NonZeroU8;
use educe::Educe;
use macro_attr_2018::macro_attr;
use nonmax::NonMaxU8;
use tuifw_screen::{self, HAlign, VAlign, Attr, Color, Event};
use tuifw_screen::{Key, Point, Rect, Thickness, Vector};
use tuifw_window::{RenderPort, Window, WindowTree};

const BG: Option<Color> = Some(Color::Black);

#[derive(Debug)]
struct Game {
    windows: Arena<GameWindow>,
    visibility: i8,
    world: World,
    close_doors: bool,
    force_show_roof: bool,
}

type WindowRender = fn(
    tree: &WindowTree<Game>,
    id: Id<GameWindow>,
    port: &mut RenderPort,
    game: &mut Game,
);

macro_attr! {
    #[derive(Educe, Component!)]
    #[educe(Debug)]
    struct GameWindow {
        window: Window,
        #[educe(Debug(ignore))]
        render: WindowRender,
    }
}

impl GameWindow {
    fn new(game: &mut Game, render: WindowRender, tree: &mut WindowTree<Game>, bounds: Rect) -> Id<Self> {
        let window = Window::new(tree, None, None, bounds);
        let game_window = game.windows.insert(|id| (GameWindow { window, render }, id));
        window.set_tag(tree, game_window);
        game_window
    }
}

fn render(
    tree: &WindowTree<Game>,
    window: Option<Window>,
    port: &mut RenderPort,
    game: &mut Game,
) {
    if let Some(window) = window {
        let window = window.tag(tree).unwrap();
        let render = game.windows[window].render;
        render(tree, window, port, game);
    } else {
        port.fill(|port, p| port.out(p, Color::White, BG, Attr::empty(), " "));
    }
}

fn switch_color(enabled: bool) -> Color {
    if enabled { Color::Green } else { Color::White }
}

fn render_status(
    _tree: &WindowTree<Game>,
    _window: Id<GameWindow>,
    port: &mut RenderPort,
    game: &mut Game,
) {
    port.out(Point { x: 0, y: 0 }, switch_color(game.close_doors), BG, Attr::empty(), "Close Doors");
    port.out(Point { x: 0, y: 1 }, switch_color(game.force_show_roof), BG, Attr::empty(), "Force Show Roof");
}

fn status_bounds(_game: &Game, visible_area_bounds: Rect) -> Rect {
    let size = Vector { x: 15, y: 2 };
    let outer_bounds = Thickness::new(0, 1, 4, 0).shrink_rect(Rect::from_tl_br(
        Point { x: 0, y: visible_area_bounds.t() },
        visible_area_bounds.bl()
    ));
    let margin = Thickness::align(size, outer_bounds.size, HAlign::Right, VAlign::Top);
    margin.shrink_rect(outer_bounds)
}

fn neg_abs(n: i16) -> i16 {
    n.checked_abs().map_or(i16::MIN, |a| -a)
}

fn norm_x_2(v: Vector) -> u32 {
    let min_c = 0u16.wrapping_sub(max(neg_abs(v.x), neg_abs(v.y)) as u16) as u32;
    let max_c = 0u16.wrapping_sub(min(neg_abs(v.x), neg_abs(v.y)) as u16) as u32;
    2 * (max_c - min_c) + 3 * min_c
}

fn render_map(
    tree: &WindowTree<Game>,
    window: Id<GameWindow>,
    port: &mut RenderPort,
    game: &mut Game,
) {
    let player = game.world.player();
    let mut visible_area = VisibleArea::new(player, game.visibility);
    game.world.render(&mut visible_area, game.force_show_roof);
    let bounds = game.windows[window].window.bounds(tree);
    let bounds = bounds.relative_to(bounds.tl);
    let center_margin = Thickness::align(Vector { x: 1, y: 1 }, bounds.size, HAlign::Center, VAlign::Center);
    let center = center_margin.shrink_rect(bounds).tl;
    for p in visible_area.bounds().points() {
        let v = p.offset_from(player);
        if norm_x_2(v) + 1 > 2 * game.visibility as u32 { continue; }
        let v = center.offset(Vector { x: 2 * v.x, y: v.y });
        let (fg, attr, ch) = match &visible_area[p] {
            Cell::Wall => render_wall(&visible_area, p),
            Cell::Roof(_) => {
                let r = &visible_area[Point { x: p.x.wrapping_add(1), y: p.y }];
                let ch = if matches!(r, Cell::Roof(_)) { "░░" } else { "░" };
                (Color::White, Attr::empty(), ch)
            },
            Cell::Vis { npc: Some(npc), .. } => {
                if npc.player {
                    (Color::Blue, Attr::empty(), "@")
                } else {
                    (Color::Green, Attr::empty(), "C")
                }
            },
            Cell::None => (Color::White, Attr::empty(), "·"),
            Cell::Vis { obj: None, .. } => (Color::White, Attr::INTENSITY, "∙"),
            Cell::Vis { obj: Some(CellObj::Door { locked }), .. } => {
                let closed = locked.is_some();
                let locked = locked.map_or(false, |x| x);
                render_door(&visible_area, p, closed, Some(locked))
            },
            &Cell::InvisDoor { closed } =>
                render_door(&visible_area, p, closed, None),
            Cell::Vis { obj: Some(CellObj::Chest { locked }), .. } => (
                if *locked { Color::Red } else { Color::Green },
                Attr::empty(),
                "▬"
            ),
        };
        port.out(v, fg, BG, attr, ch);
    }
}

fn render_wall(
    visible_area: &VisibleArea,
    p: Point,
) -> (Color, Attr, &'static str) {

    fn is_wall(cell: &Cell) -> u8 {
        matches!(cell, Cell::Wall) as u8
    }

    fn is_door(cell: &Cell) -> u8 {
        matches!(
            cell,
            Cell::InvisDoor { .. } | Cell::Vis { obj: Some(CellObj::Door { .. }), .. }
        ) as u8
    }

    fn is_wall_x(cell: &Cell) -> u8 {
        matches!(cell, Cell::Roof(CellRoof::Wall)) as u8
    }

    fn is_door_x(cell: &Cell) -> u8 {
        matches!( cell, Cell::Roof(CellRoof::Door)) as u8
    }

    let r = &visible_area[Point { x: p.x.wrapping_add(1), y: p.y }];
    let d = &visible_area[Point { x: p.x, y: p.y.wrapping_add(1) }];
    let l = &visible_area[Point { x: p.x.wrapping_sub(1), y: p.y }];
    let u = &visible_area[Point { x: p.x, y: p.y.wrapping_sub(1) }];
    let index
        = (is_wall(l) << 0) | (is_door(l) << 1) | (is_wall(u) << 2) | (is_door(u) << 3)
        | (is_wall(r) << 4) | (is_door(r) << 5) | (is_wall(d) << 6) | (is_door(d) << 7)
    ;
    let index = if index == 0 {
        (is_wall_x(l) << 0) | (is_door_x(l) << 1) | (is_wall_x(u) << 2) | (is_door_x(u) << 3) |
        (is_wall_x(r) << 4) | (is_door_x(r) << 5) | (is_wall_x(d) << 6) | (is_door_x(d) << 7)
    } else {
        index
    };

    const WALL: [&'static str; 256] = [
        "│ ", "─ ", "┤ ", "┤ ", "┴ ", "┘ ", "┘ ", "┘ ",  "┴ ", "┘ ", "┘ ", "┘ ", "┴ ", "┘ ", "┘ ", "┘ ",
        "──", "──", "──", "──", "└─", "┴─", "┴─", "┴─",  "└─", "──", "──", "──", "└─", "──", "──", "──",
        "├ ", "─ ", "┼ ", "┼ ", "└ ", "┴ ", "┴ ", "┴ ",  "└ ", "─ ", "┴ ", "┴ ", "└ ", "─ ", "┴ ", "┴ ",
        "├ ", "─ ", "┼ ", "┼ ", "└ ", "┴ ", "┴ ", "┴ ",  "└ ", "─ ", "┴ ", "┴ ", "└ ", "─ ", "┴ ", "┴ ",

        "┬ ", "┐ ", "┐ ", "┐ ", "│ ", "┤ ", "┤ ", "┤ ",  "│ ", "┐ ", "┐ ", "┐ ", "│ ", "┐ ", "┐ ", "┐ ",
        "┌─", "┬─", "┬─", "┬─", "├─", "┼─", "┼─", "┼─",  "┌─", "┬─", "┬─", "┬─", "┌─", "┬─", "┬─", "┬─",
        "┌ ", "┬ ", "┬ ", "┬ ", "├ ", "┼ ", "┼ ", "┼ ",  "┌ ", "┬ ", "┬ ", "┬ ", "┌ ", "┬ ", "┬ ", "┬ ",
        "┌ ", "┬ ", "┬ ", "┬ ", "├ ", "┼ ", "┼ ", "┼ ",  "┌ ", "┬ ", "┬ ", "┬ ", "┌ ", "┬ ", "┬ ", "┬ ",

        "┬ ", "┐ ", "┐ ", "┐ ", "│ ", "┘ ", "┘ ", "┘ ",  "│ ", "─ ", "┤ ", "┤ ", "│ ", "─ ", "┤ ", "┤ ",
        "┌─", "──", "──", "──", "└─", "┴─", "┴─", "┴─",  "──", "──", "──", "──", "──", "──", "──", "──",
        "┌ ", "─ ", "┬ ", "┬ ", "└ ", "┴ ", "┴ ", "┴ ",  "├ ", "─ ", "┼ ", "┼ ", "├ ", "─ ", "┼ ", "┼ ",
        "┌ ", "─ ", "┬ ", "┬ ", "└ ", "┴ ", "┴ ", "┴ ",  "├ ", "─ ", "┼ ", "┼ ", "├ ", "─ ", "┼ ", "┼ ",

        "┬ ", "┐ ", "┐ ", "┐ ", "│ ", "┘ ", "┘ ", "┘ ",  "│ ", "─ ", "┤ ", "┤ ", "│ ", "─ ", "┤ ", "┤ ",
        "┌─", "──", "──", "──", "└─", "┴─", "┴─", "┴─",  "──", "──", "──", "──", "──", "──", "──", "──",
        "┌ ", "─ ", "┬ ", "┬ ", "└ ", "┴ ", "┴ ", "┴ ",  "├ ", "─ ", "┼ ", "┼ ", "├ ", "─ ", "┼ ", "┼ ",
        "┌ ", "─ ", "┬ ", "┬ ", "└ ", "┴ ", "┴ ", "┴ ",  "├ ", "─ ", "┼ ", "┼ ", "├ ", "─ ", "┼ ", "┼ ",
    ];

    (Color::White, Attr::empty(), WALL[index as usize])
}

fn render_door(
    visible_area: &VisibleArea,
    p: Point,
    closed: bool,
    locked: Option<bool>
) -> (Color, Attr, &'static str) {
    let horizontal = !closed ^ {
        let h1 = &visible_area[Point { x: p.x.wrapping_add(1), y: p.y }];
        let h1 = matches!(h1, Cell::Wall | Cell::Vis { obj: Some(CellObj::Door { .. }), .. });
        if !h1 {
            let v1 = &visible_area[Point { x: p.x, y: p.y.wrapping_add(1) }];
            let v1 = matches!(v1, Cell::Wall | Cell::Vis { obj: Some(CellObj::Door { .. }), .. });
            if !v1 {
                let h2 = &visible_area[Point { x: p.x.wrapping_sub(1), y: p.y }];
                matches!(h2, Cell::Wall | Cell::Vis { obj: Some(CellObj::Door { .. }), .. })
            } else {
                false
            }
        } else {
            true
        }
    };
    let ch = if horizontal { "─" } else { "|" };
    match locked {
        None => (Color::White, Attr::empty(), ch),
        Some(false) => (Color::Green, Attr::empty(), ch),
        Some(true) => (Color::Red, Attr::empty(), ch)
    }
}

fn map_bounds(game: &Game, screen_size: Vector) -> Rect {
    let visible_area_size = VisibleArea::size(game.visibility);
    let margin = Thickness::align(
        Vector {
            x: (2 * (visible_area_size as u16 - 1) + 1) as i16,
            y: visible_area_size as u16 as i16
        },
        screen_size, HAlign::Center, VAlign::Center
    );
    margin.shrink_rect(Rect { tl: Point { x: 0, y: 0 }, size: screen_size })
}

struct PlayerData {
    desired_movement_direction: Option<Direction>,
    wants_close_door: bool,
}

struct Player;

const PLAYER: Player = Player;

impl Ai for Player {
    fn desired_movement_direction(&self, data: &mut dyn Any) -> Option<Direction> {
        let data = data.downcast_mut::<PlayerData>().unwrap();
        data.desired_movement_direction
    }

    fn wants_close_door(&self, data: &mut dyn Any) -> bool {
        let data = data.downcast_mut::<PlayerData>().unwrap();
        data.wants_close_door
    }
}

fn door_offset(w: NonZeroU8, h: NonZeroU8, door: u16) -> Vector {
    let door = door % (2 * w.get() as u16 + 2 * h.get() as u16);
    if door < h.get() as u16 {
        Vector {
            x: 0,
            y: door as i16
        }
    } else if door < h.get() as u16 + w.get() as u16 {
        Vector {
            x: (door - h.get() as u16) as i16,
            y: (h.get() - 1) as u16 as i16
        }
    } else if door < 2 * h.get() as u16 + w.get() as u16 {
        Vector {
            x: (w.get() - 1) as u16 as i16, 
            y: (
                (h.get() - 1) as u16 -
                (door - h.get() as u16 - w.get() as u16)
            ) as i16
        }
    } else {
        Vector {
            x: (
                (w.get() - 1) as u16 -
                (door - 2 * h.get() as u16 - w.get() as u16)
            ) as i16,
            y: 0
        }
    }
}

macro_rules! nz {
    (
        0
    ) => {
        compile_error!("zero");
    };
    (
        0u8
    ) => {
        compile_error!("zero");
    };
    (
        $v:literal
    ) => {
        (unsafe { NonZeroU8::new_unchecked($v) })
    };
}

macro_rules! nm {
    (
        255
    ) => {
        compile_error!("max");
    };
    (
        255u8
    ) => {
        compile_error!("max");
    };
    (
        $v:literal
    ) => {
        (unsafe { NonMaxU8::new_unchecked($v) })
    };
}

fn add_building(world: &mut World, tl: Point, w: NonZeroU8, h: NonZeroU8, door: u16, locked: NonMaxU8) {
    let roof_group = world.add_group();
    let door = tl.offset(door_offset(w, h, door));
    let bounds = Rect { tl, size: Vector { x: w.get() as u16 as i16, y: h.get() as u16 as i16 } };
    world.add_obj(None, bounds.l_line(), ObjData::Wall { outer: true });
    world.add_obj(None, bounds.t_line(), ObjData::Wall { outer: true });
    world.add_obj(None, bounds.r_line(), ObjData::Wall { outer: true });
    world.add_obj(None, bounds.b_line(), ObjData::Wall { outer: true });
    world.add_obj(Some(roof_group), bounds, ObjData::Roof);
    world.add_obj(None, Rect { tl: door, size: Vector { x: 1, y: 1 } }, ObjData::Door(Door {
        locked: Some(locked),
        key: 0
    }));
}

mod imperial_office;

#[start]
fn main(_: isize, _: *const *const u8) -> isize {
    let screen = unsafe { tuifw_screen::init() }.unwrap();
    let mut world = World::new(Npc {
        race: Race::Danmer,
        gender: Gender::Female,
        class: NpcClass::Mercenery,
        ai: &PLAYER,
        ai_data: Box::new(PlayerData {
            desired_movement_direction: None,
            wants_close_door: false
        }),
        movement_priority: 0,
    });
    add_building(&mut world, Point { x: -5, y: 0 }, nz!(5), nz!(7), 14, nm!(0));
    add_building(&mut world, Point { x: 4, y: 1 }, nz!(5), nz!(7), 2, nm!(1));
    imperial_office::add_imperial_office(&mut world);
    world.add_obj(None, Rect { tl: Point { x: -4, y: 1 }, size: Vector { x: 1, y: 1 } }, ObjData::Chest(Chest {
        locked: nm!(0),
        key: 0,
    }));
    let mut windows = WindowTree::new(screen, render);
    let mut game = Game {
        windows: Arena::new(),
        visibility: 10,
        world,
        close_doors: false,
        force_show_roof: false,
    };
    let map_initial_bounds = map_bounds(&game, windows.screen_size());
    let map = GameWindow::new(&mut game, render_map, &mut windows, map_initial_bounds);
    let status_initial_bounds = status_bounds(&game, map_initial_bounds);
    let status = GameWindow::new(&mut game, render_status, &mut windows, status_initial_bounds);
    loop {
        let event = loop {
            if let Some(event) = WindowTree::update(&mut windows, true, &mut game).unwrap() {
                break event;
            }
        };
        if let Event::Resize = event {
            let map_bounds = map_bounds(&game, windows.screen_size());
            let status_bounds = status_bounds(&game, map_bounds);
            let map = game.windows[map].window;
            let status = game.windows[status].window;
            map.move_xy(&mut windows, map_bounds);
            status.move_xy(&mut windows, status_bounds);
        }
        match event {
            Event::Key(n, Key::Char('f')) if n.get() % 2 != 0 => {
                game.force_show_roof = !game.force_show_roof;
                game.windows[status].window.invalidate(&mut windows);
                game.windows[map].window.invalidate(&mut windows);
            },
            Event::Key(n, Key::Char('c')) if n.get() % 2 != 0 => {
                let close_doors = !game.close_doors;
                game.close_doors = close_doors;
                game.world.player_data_mut::<PlayerData>().wants_close_door = close_doors;
                game.windows[status].window.invalidate(&mut windows);
            },
            _ => { }
        }
        let movement = match event {
            Event::Key(_, Key::Escape) => break,
            Event::Key(n, Key::Right) => Some((n, Direction::E)),
            Event::Key(n, Key::Left) => Some((n, Direction::W)),
            Event::Key(n, Key::Up) => Some((n, Direction::N)),
            Event::Key(n, Key::Down) => Some((n, Direction::S)),
            _ => None,
        };
        if let Some((n, direction)) = movement {
            game.world.player_data_mut::<PlayerData>().desired_movement_direction = Some(direction);
            for _ in 0 .. n.get() {
                game.world.step();
            }
            game.windows[map].window.invalidate(&mut windows);
        }
    }
    0
}

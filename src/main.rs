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
    game.world.render(&mut visible_area);
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
            Cell::Invis(CellInvis::Roof) => {
                let r = &visible_area[Point { x: p.x.wrapping_add(1), y: p.y }];
                let ch = if matches!(r, Cell::Invis(CellInvis::Roof)) { "░░" } else { "░" };
                (Color::White, Attr::empty(), ch)
            },
            Cell::Vis { npc: Some(npc), .. } => {
                if npc.player {
                    (Color::Blue, Attr::empty(), "@")
                } else {
                    (Color::Green, Attr::empty(), "C")
                }
            },
            Cell::Invis(CellInvis::None) => (Color::White, Attr::empty(), "·"),
            Cell::Vis { obj: None, .. } => (Color::White, Attr::INTENSITY, "∙"),
            Cell::Vis { obj: Some(CellObj::Door { locked }), .. } => {
                let closed = locked.is_some();
                let locked = locked.map_or(false, |x| x);
                render_door(&visible_area, p, closed, Some(locked))
            },
            &Cell::Invis(CellInvis::Door { closed }) =>
                render_door(&visible_area, p, closed, None),
            Cell::Vis { obj: Some(CellObj::Chest { locked }), .. } => (
                if *locked { Color::Red } else { Color::Green },
                Attr::empty(),
                "■"
            ),
        };
        port.out(v, fg, BG, attr, ch);
    }
}

fn is_wall(cell: &Cell) -> bool {
    matches!(cell, Cell::Wall)
}

fn is_door(cell: &Cell) -> bool {
    matches!(
        cell,
        Cell::Invis(CellInvis::Door { .. }) | Cell::Vis { obj: Some(CellObj::Door { .. }), .. }
    )
}

fn render_wall(
    visible_area: &VisibleArea,
    p: Point,
) -> (Color, Attr, &'static str) {
    let r = &visible_area[Point { x: p.x.wrapping_add(1), y: p.y }];
    let d = &visible_area[Point { x: p.x, y: p.y.wrapping_add(1) }];
    let l = &visible_area[Point { x: p.x.wrapping_sub(1), y: p.y }];
    let u = &visible_area[Point { x: p.x, y: p.y.wrapping_sub(1) }];
    let l = (is_wall(l) || is_door(l)) as u8;
    let u = (is_wall(u) || is_door(u)) as u8;
    let r = if is_wall(r) {
        0
    } else if is_door(r) {
        1
    } else {
        2
    };
    let d = (is_wall(d) || is_door(d)) as u8;
    let index = (r << 3) | (l << 2) | (u << 1) | d;
    let ch = [
        "──", "┌─", "└─", "├─", "──", "┬─", "┴─", "┼─",
        "─", "┌", "└", "├", "─", "┬", "┴", "┼",
        "│", "┬", "┴", "│", "─", "┐", "┘", "┤",
    ][index as usize];
    (Color::White, Attr::empty(), ch)
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

fn add_building(world: &mut World, tl: Point, w: NonZeroU8, h: NonZeroU8, door: u16) {
    let door = tl.offset(door_offset(w, h, door));
    let bounds = Rect { tl, size: Vector { x: w.get() as u16 as i16, y: h.get() as u16 as i16 } };
    world.add(bounds.l_line(), ObjData::Wall);
    world.add(bounds.t_line(), ObjData::Wall);
    world.add(bounds.r_line(), ObjData::Wall);
    world.add(bounds.b_line(), ObjData::Wall);
    world.add(Rect { tl: door, size: Vector { x: 1, y: 1 } }, ObjData::Door(Door {
        locked: Some(unsafe { NonMaxU8::new_unchecked(0) }),
        key: 0
    }));
}

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
    add_building(&mut world, Point { x: -5, y: 0 }, nz!(5), nz!(7), 14);
    add_building(&mut world, Point { x: 4, y: 1 }, nz!(5), nz!(7), 2);
    add_building(&mut world, Point { x: -2, y: 11 }, nz!(12), nz!(7), 28);
    let mut windows = WindowTree::new(screen, render);
    let mut game = Game {
        windows: Arena::new(),
        visibility: 10,
        world
    };
    let map_initial_bounds = map_bounds(&game, windows.screen_size());
    let map = GameWindow::new(&mut game, render_map, &mut windows, map_initial_bounds);
    loop {
        let event = loop {
            if let Some(event) = WindowTree::update(&mut windows, true, &mut game).unwrap() {
                break event;
            }
        };
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

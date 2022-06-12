#![deny(warnings)]

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

use components_arena::{Arena, Component, Id};
use core::cmp::{max, min};
use core::num::NonZeroU8;
use educe::Educe;
use macro_attr_2018::macro_attr;
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
        let (fg, attr, ch) = match visible_area[p] {
            Cell { player: true, .. } => (Color::Blue, Attr::empty(), "@"),
            Cell { wall: Wall::None, is_visible: false, .. } => (Color::White, Attr::empty(), "·"),
            Cell { wall: Wall::None, is_visible: true, .. } => (Color::White, Attr::INTENSITY, "∙"),
            Cell { wall: Wall::Roof, is_visible: false, .. } => {
                let r = &visible_area[Point { x: p.x.wrapping_add(1), y: p.y }].wall;
                let ch = if matches!(r, Wall::Roof) { "░░" } else { "░" };
                (Color::White, Attr::empty(), ch)
            },
            Cell { wall: Wall::Roof, is_visible: true, .. } => (Color::White, Attr::INTENSITY, "∙"),
            Cell { wall: Wall::Door { opened }, .. } => {
                let horizontal = opened ^ {
                    let h1 = !visible_area[Point { x: p.x.wrapping_add(1), y: p.y }].wall.is_none();
                    if !h1 {
                        let v1 = !visible_area[Point { x: p.x, y: p.y.wrapping_add(1) }].wall.is_none();
                        if !v1 {
                            !visible_area[Point { x: p.x.wrapping_sub(1), y: p.y }].wall.is_none()
                        } else {
                            false
                        }
                    } else {
                        true
                    }
                };
                (Color::Green, Attr::empty(), if horizontal { "─" } else { "|" })
            },
            Cell { wall: Wall::Wall, .. } => {
                let r = &visible_area[Point { x: p.x.wrapping_add(1), y: p.y }].wall;
                let d = &visible_area[Point { x: p.x, y: p.y.wrapping_add(1) }].wall;
                let l = &visible_area[Point { x: p.x.wrapping_sub(1), y: p.y }].wall;
                let u = &visible_area[Point { x: p.x, y: p.y.wrapping_sub(1) }].wall;
                let l = !l.is_none() as u8;
                let u = !u.is_none() as u8;
                let r = if matches!(r, Wall::None | Wall::Roof) {
                    0
                } else if matches!(r, Wall::Door { .. }) {
                    1
                } else {
                    2
                };
                let d = !d.is_none() as u8;
                let index = (r << 3) | (l << 2) | (u << 1) | d;
                let ch = [
                    "│", "┬", "┴", "│", "─", "┐", "┘", "┤",
                    "─", "┌", "└", "├", "─", "┬", "┴", "┼",
                    "──", "┌─", "└─", "├─", "──", "┬─", "┴─", "┼─",
                ][index as usize];
                (Color::White, Attr::empty(), ch)
            },
        };
        port.out(v, fg, BG, attr, ch);
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

#[start]
fn main(_: isize, _: *const *const u8) -> isize {
    let screen = unsafe { tuifw_screen::init() }.unwrap();
    let mut world = World::new();
    world.add_building(Point { x: -5, y: 0 }, nz!(5), nz!(7), 14);
    world.add_building(Point { x: 4, y: 1 }, nz!(5), nz!(7), 2);
    world.add_building(Point { x: -2, y: 11 }, nz!(12), nz!(7), 28);
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
        let step = match event {
            Event::Key(_, Key::Escape) => break,
            Event::Key(n, Key::Right) => Vector { x: (n.get() as i16), y: 0 },
            Event::Key(n, Key::Left) => Vector { x: (n.get() as i16).wrapping_neg(), y: 0 },
            Event::Key(n, Key::Up) => Vector { y: (n.get() as i16).wrapping_neg(), x: 0 },
            Event::Key(n, Key::Down) => Vector { y: n.get() as i16, x: 0 },
            _ => Vector { x: 0, y: 0 },
        };
        game.world.move_player(game.world.player().offset(step));
        game.windows[map].window.invalidate(&mut windows);
    }
    0
}

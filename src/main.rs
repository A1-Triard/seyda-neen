#![deny(warnings)]
#![allow(dead_code)]

#![windows_subsystem="console"]

use components_arena::{Arena, Component, Id};
use macro_attr_2018::macro_attr;
use std::cmp::{max, min};
use tuifw_screen::{self, HAlign, VAlign, Attr, Color, Event};
use tuifw_screen::{Key, Point, Range1d, Rect, Thickness, Vector};
use tuifw_window::{RenderPort, Window, WindowTree};

const BG: Option<Color> = Some(Color::Black);

struct Game {
    windows: Arena<GameWindow>,
    player: Point,
    visibility: u16,
}

type WindowRender = fn(
    tree: &WindowTree<Game>,
    id: Id<GameWindow>,
    port: &mut RenderPort,
    game: &mut Game,
);

macro_attr! {
    #[derive(Component!)]
    struct GameWindow {
        window: Window,
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
    assert_eq!(v.x % 2, 0);
    let min_c = 0u16.wrapping_sub(max(neg_abs(v.x) / 2, neg_abs(v.y)) as u16) as u32;
    let max_c = 0u16.wrapping_sub(min(neg_abs(v.x) / 2, neg_abs(v.y)) as u16) as u32;
    2 * (max_c - min_c) + 3 * min_c
}

fn render_map(
    tree: &WindowTree<Game>,
    window: Id<GameWindow>,
    port: &mut RenderPort,
    game: &mut Game,
) {
    let bounds = game.windows[window].window.bounds(tree);
    let bounds = bounds.relative_to(bounds.tl);
    let player_margin = Thickness::align(Vector { x: 1, y: 1 }, bounds.size, HAlign::Center, VAlign::Center);
    let player_bounds = player_margin.shrink_rect(bounds);
    let map_size = Vector {
        x: game.visibility.saturating_mul(4).saturating_add(1) as i16,
        y: game.visibility.saturating_mul(2).saturating_add(1) as i16
    };
    let map_padding = Thickness::align(player_bounds.size, map_size, HAlign::Center, VAlign::Center);
    let map_bounds = map_padding.expand_rect(player_bounds);
    for x in Range1d::new(map_bounds.l(), map_bounds.r()).step_by(2) {
        for y in Range1d::new(map_bounds.t(), map_bounds.b()) {
            if norm_x_2(player_bounds.tl.offset_from(Point { x, y })) + 1 <= 2 * game.visibility as u32 {
                port.out(Point { x, y }, Color::White, BG, Attr::empty(), "·");
            }
        }
    }
    port.out(player_bounds.tl, Color::Blue, BG, Attr::empty(), "@");
}

fn map_bounds(game: &Game, screen_size: Vector) -> Rect {
    let map_size = Vector {
        x: game.visibility.saturating_mul(4).saturating_add(1) as i16,
        y: game.visibility.saturating_mul(2).saturating_add(1) as i16
    };
    let margin = Thickness::align(map_size, screen_size, HAlign::Center, VAlign::Center);
    margin.shrink_rect(Rect { tl: Point { x: 0, y: 0 }, size: screen_size })
}

fn main() {
    let screen = unsafe { tuifw_screen::init() }.unwrap();
    let mut windows = WindowTree::new(screen, render);
    let mut game = Game {
        windows: Arena::new(),
        player: Point { x: 0, y: 0 },
        visibility: 10,
    };
    let map_initial_bounds = map_bounds(&game, windows.screen_size());
    let _map = GameWindow::new(&mut game, render_map, &mut windows, map_initial_bounds);
    loop {
        let event = WindowTree::update(&mut windows, true, &mut game).unwrap().unwrap();
        match event {
            Event::Key(_, Key::Escape) => break,
            _ => { },
        }
    }
}

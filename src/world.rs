use alloc::boxed::Box;
use alloc::vec;
use alloc::vec::Vec;
use components_arena::{Arena, Component, Id};
use core::any::Any;
use core::cmp::max;
use core::mem::replace;
use core::ops::{Index, IndexMut};
use educe::Educe;
use either::{Either, Left, Right};
use macro_attr_2018::macro_attr;
use nonmax::NonMaxU8;
use rand::{Rng, SeedableRng};
use rand::rngs::SmallRng;
use tuifw_screen::{HAlign, VAlign, Point, Rect, Thickness, Vector};

const SECTOR_OBJS_MAX: usize = 32;
const SECTOR_AREA_MIN: u32 = 16;

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Hash)]
pub enum Race {
    Danmer,
    Altmer,
    Bosmer,
    Redguard,
    Nord,
    Imperial,
    Breton,
    Khajiit,
    Argonian,
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Hash)]
pub enum Gender {
    Male,
    Female
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Hash)]
pub enum NpcClass {
    Artisan,
    Noble,
    Smith,
    Mercenery,
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Hash)]
pub enum Direction {
    N, S, W, E,
    NW, NE, SW, SE
}

impl Direction {
    pub fn vector(self) -> Vector {
        match self {
            Direction::N => Vector { x: 0, y: -1 },
            Direction::S => Vector { x: 0, y: 1 },
            Direction::W => Vector { x: -1, y: 0 },
            Direction::E => Vector { x: 1, y: 0 },
            Direction::NW => Vector { x: -1, y: -1 },
            Direction::NE => Vector { x: 1, y: -1 },
            Direction::SW => Vector { x: -1, y: 1 },
            Direction::SE => Vector { x: 1, y: 1 },
        }
    }
}

pub trait Ai {
    fn desired_movement_direction(&self, data: &mut dyn Any) -> Option<Direction>;
    fn wants_close_door(&self, data: &mut dyn Any) -> bool;
}

#[derive(Educe)]
#[educe(Debug)]
pub struct Npc {
    pub race: Race,
    pub gender: Gender,
    pub class: NpcClass,
    #[educe(Debug(ignore))]
    pub ai: &'static dyn Ai,
    pub ai_data: Box<dyn Any>,
    pub movement_priority: i8,
}

#[derive(Debug)]
pub struct Door {
    pub locked: Option<NonMaxU8>,
    pub key: u16,
}

#[derive(Debug)]
pub struct Chest {
    pub locked: NonMaxU8,
    pub key: u16,
}

#[derive(Debug)]
pub enum Obj {
    Door(Door),
    Chest(Chest),
    Npc(Npc),
    Wall,
    Roof,
}

macro_attr! {
    #[derive(Debug, Component!)]
    struct WorldObj {
        obj: Obj,
        bounds: Rect
    }
}

macro_attr! {
    #[derive(Debug, Component!)]
    struct Sector {
        bounds: Rect,
        data: Either<[Id<Sector>; 4], Vec<Id<WorldObj>>>,
    }
}

impl Sector {
    fn split_if_needed(this: Id<Self>, sectors: &mut Arena<Sector>, objs: &mut Arena<WorldObj>) {
        let sector = &mut sectors[this];
        if sector.bounds.area() <= SECTOR_AREA_MIN { return; }
        let sector_objs = if let Right(objs) = sector.data.as_mut() {
            if objs.len() < SECTOR_OBJS_MAX { return; }
            replace(objs, Vec::new())
        } else {
            return;
        };
        let center_x = sector.bounds.l() + sector.bounds.w() / 2;
        let center_y = sector.bounds.t() + sector.bounds.h() / 2;
        let subsectors = [
            Rect::from_tl_br(
                Point { x: center_x, y: sector.bounds.t() },
                Point { x: sector.bounds.r(), y: center_y }
            ),
            Rect::from_tl_br(
                Point { x: sector.bounds.l(), y: sector.bounds.t() },
                Point { x: center_x, y: center_y }
            ),
            Rect::from_tl_br(
                Point { x: sector.bounds.l(), y: center_y },
                Point { x: sector.bounds.l(), y: sector.bounds.b() }
            ),
            Rect::from_tl_br(
                Point { x: center_x, y: center_y },
                Point { x: sector.bounds.r(), y: sector.bounds.b() }
            )
        ].map(|bounds| {
            let mut subsector_objs = Vec::new();
            for &obj in &sector_objs {
                if !objs[obj].bounds.intersect(bounds).is_empty() {
                    subsector_objs.push(obj);
                }
            }
            sectors.insert(|id| (Sector { bounds, data: Right(subsector_objs) }, id))
        });
        sectors[this].data = Left(subsectors);
        for subsector in subsectors {
            Self::split_if_needed(subsector, sectors, objs);
        }
    }
}

#[derive(Debug)]
pub struct World {
    rng: SmallRng,
    player: Id<WorldObj>,
    objs: Arena<WorldObj>,
    sectors: Arena<Sector>,
    area: [Id<Sector>; 4],
}

impl World {
    pub fn new(player: Npc) -> Self {
        let mut objs = Arena::new();
        let player = objs.insert(|id| (WorldObj {
            obj: Obj::Npc(player),
            bounds: Rect { tl: Point { x: 0, y: 0 }, size: Vector { x: 1, y: 1 } },
        }, id));
        let mut sectors = Arena::new();
        let tl = sectors.insert(|id| (Sector {
            bounds: Rect::from_tl_br(
                Point { x: i16::MIN, y: i16::MIN },
                Point { x: 0, y: 0 }
            ),
            data: Right(Vec::new())
        }, id));
        let tr = sectors.insert(|id| (Sector {
            bounds: Rect::from_tl_br(
                Point { x: 0, y: i16::MIN },
                Point { x: i16::MAX, y: 0 }
            ),
            data: Right(Vec::new())
        }, id));
        let bl = sectors.insert(|id| (Sector {
            bounds: Rect::from_tl_br(
                Point { x: i16::MIN, y: 0 },
                Point { x: 0, y: i16::MAX }
            ),
            data: Right(Vec::new())
        }, id));
        let br = sectors.insert(|id| (Sector {
            bounds: Rect::from_tl_br(
                Point { x: 0, y: 0 },
                Point { x: i16::MAX, y: i16::MAX }
            ),
            data: Right(vec![player])
        }, id));
        World { rng: SmallRng::from_entropy(), player, objs, sectors, area: [tr, tl, bl, br] }
    }

    fn bounds(&self, obj: Id<WorldObj>) -> Rect {
        self.objs[obj].bounds
    }

    pub fn player(&self) -> Point {
        self.bounds(self.player).tl
    }

    pub fn player_data_mut<T: Any>(&mut self) -> &mut T {
        match &mut self.objs[self.player].obj {
            Obj::Npc(Npc { ai_data, .. }) => ai_data.downcast_mut().unwrap(),
            _ => unreachable!()
        }
    }

    pub fn add(&mut self, bounds: Rect, obj: Obj) {
        let obj = self.objs.insert(|id|
            (WorldObj { bounds, obj }, id)
        );
        for p in bounds.points() {
            let (sector, sector_objs) = self.sector_mut(p);
            if !sector_objs.iter().any(|&x| x == obj) {
                sector_objs.push(obj);
                Sector::split_if_needed(sector, &mut self.sectors, &mut self.objs);
            }
        }
    }

    fn sector(&self, p: Point) -> (Id<Sector>, &Vec<Id<WorldObj>>) {
        let mut subsectors = &self.area;
        loop {
            let &sector = subsectors.into_iter().find(|&&x| self.sectors[x].bounds.contains(p)).unwrap();
            match self.sectors[sector].data.as_ref() {
                Right(x) => break (sector, x),
                Left(x) => subsectors = x
            }
        }
    }

    fn sector_mut(&mut self, p: Point) -> (Id<Sector>, &mut Vec<Id<WorldObj>>) {
        let mut subsectors = self.area.clone();
        let (id, obj) = loop {
            let sector = subsectors.into_iter().find(|&x| self.sectors[x].bounds.contains(p)).unwrap();
            match self.sectors[sector].data.as_mut() {
                Right(x) => break (sector, x as *mut _),
                Left(x) => subsectors = x.clone()
            }
        };
        (id, unsafe { &mut *obj })
    }

    fn objs(&self, p: Point) -> impl Iterator<Item=(Id<WorldObj>, &Obj)> {
        self.sector(p).1.iter().filter_map(move |&world_obj| {
            let world_obj_data = &self.objs[world_obj];
            if world_obj_data.bounds.contains(p) {
                Some((world_obj, &world_obj_data.obj))
            } else {
                None
            }
        })
    }

    fn is_barrier(&self, p: Point) -> bool {
        self.objs(p).any(|(_, x)| matches!(x, Obj::Wall | Obj::Door(Door { locked: Some(_), .. })))
    }

    pub fn step(&mut self) {
        struct NpcMove {
            id: Id<WorldObj>,
            from: Point,
            to: Option<Point>,
            ai: &'static dyn Ai,
            wants_close_door: bool,
            movement_order: u16,
        }
        let mut npcs = self.objs.items().iter()
            .filter_map(|(id, obj)|
                if let WorldObj { obj: Obj::Npc(npc), bounds } = obj {
                    let movement_order = i8::MAX.wrapping_sub(npc.movement_priority) as u8;
                    let movement_order = ((movement_order as u16) << 8) | (self.rng.gen::<u8>() as u16);
                    Some(NpcMove {
                        id,
                        from: bounds.tl,
                        ai: npc.ai,
                        to: None,
                        wants_close_door: false,
                        movement_order,
                    })
                } else {
                    None
                }
            )
            .collect::<Vec<_>>();
        for npc in &mut npcs {
            let ai_data = if let Obj::Npc(npc) = &mut self.objs[npc.id].obj {
                npc.ai_data.as_mut()
            } else {
                unreachable!()
            };
            npc.to = npc.ai.desired_movement_direction(ai_data).map(|direction|
                npc.from.offset(direction.vector())
            );
            npc.wants_close_door = npc.ai.wants_close_door(ai_data);
        }
        for npc in &mut npcs {
            if let Some(to) = npc.to {
                let to_barrier = self.objs(to).find(|(_, x)| matches!(x, Obj::Wall | Obj::Door(_)));
                if let Some((to_barrier, _)) = to_barrier {
                    match &mut self.objs[to_barrier].obj {
                        Obj::Wall => npc.to = None,
                        Obj::Door(door) => {
                            let open = if let Some(locked) = door.locked {
                                if locked.get() != 0 {
                                    npc.to = None;
                                    false
                                } else {
                                    true
                                }
                            } else {
                                false
                            };
                            if open { door.locked = None; }
                        },
                        _ => unreachable!(),
                    }
                }
            }
        }
        npcs.sort_by_key(|x| x.movement_order);
        for npc in npcs {
            if let Some(to) = npc.to {
                if self.objs(to).any(|(_, x)| matches!(x, Obj::Npc(_))) { continue; }
                if npc.wants_close_door {
                    let from_door = self.objs(npc.from).find(|(_, x)| matches!(x, Obj::Door(_))).map(|x| x.0);
                    if let Some(from_door) = from_door {
                        match &mut self.objs[from_door].obj {
                            Obj::Door(door) => {
                                if door.locked.is_none() {
                                    door.locked = Some(unsafe { NonMaxU8::new_unchecked(0) });
                                }
                            },
                            _ => unreachable!(),
                        }
                    }
                }
                self.objs[npc.id].bounds.tl = to;
            }
        }
    }

    pub fn render(&self, area: &mut VisibleArea) {
        let player = self.player();
        for p in area.bounds().points() {
            let mut wall = false;
            let mut door = false;
            let mut roof = false;
            let mut vis_obj = None;
            let mut vis_npc = None;
            for (_, obj) in self.objs(p) {
                match obj {
                    Obj::Roof => roof = true,
                    Obj::Wall => wall = true,
                    Obj::Door(door_obj) => {
                        door = true;
                        vis_obj = Some(CellObj::Door { locked: door_obj.locked.map(|x| x.get() != 0) });
                    },
                    Obj::Chest(chest) => vis_obj = Some(CellObj::Chest { locked: chest.locked.get() != 0 }),
                    Obj::Npc(npc) => vis_npc = Some(CellNpc {
                        player: p == player,
                        race: npc.race,
                        gender: npc.gender,
                        class: npc.class
                    }),
                }
            }
            area[p] = if wall {
                Cell::Wall
            } else if !self.is_visible_from(player, p) {
                if door { Cell::Wall } else { Cell::Invis { roof } }
            } else {
                Cell::Vis { obj: vis_obj, npc: vis_npc }
            };
        }
    }

    fn is_visible_from(&self, origin: Point, p: Point) -> bool {
        let offset = p.offset_from(origin);
        if offset.is_null() { return true; }
        let diagonals = 0u16.wrapping_sub(max(neg_abs(offset.x), neg_abs(offset.y)) as u16);
        let diagonal = Vector { x: offset.x.signum(), y: offset.y.signum() };
        let diagonal_sum = Vector {
            x: diagonal.x.wrapping_mul(diagonals as i16),
            y: diagonal.y.wrapping_mul(diagonals as i16),
        };
        let straight_sum = offset - diagonal_sum;
        debug_assert!(straight_sum.x == 0 || straight_sum.y == 0);
        let straights = 0u16.wrapping_sub((neg_abs(straight_sum.x) + neg_abs(straight_sum.y)) as u16);
        let straight = Vector { x: straight_sum.x.signum(), y: straight_sum.y.signum() };
        debug_assert_eq!(straight.x.wrapping_mul(straights as i16), straight_sum.x);
        debug_assert_eq!(straight.y.wrapping_mul(straights as i16), straight_sum.y);
        debug_assert_eq!(diagonal_sum + straight_sum, offset);
        debug_assert_ne!(diagonals + straights, 0);
        debug_assert!(!diagonal.is_null());
        let mut f = origin;
        let mut b = p;
        for _ in 0 .. diagonals {
            f = f.offset(diagonal);
            if f == p { return true; }
            if self.is_barrier(f) { return false; }
            b = b.offset(-diagonal);
            if self.is_barrier(b) { return false; }
        }
        for _ in 0 .. straights {
            f = f.offset(straight);
            if f == p { return true; }
            if self.is_barrier(f) { return false; }
            b = b.offset(-straight);
            if self.is_barrier(b) { return false; }
        }
        true
    }
}

fn neg_abs(n: i16) -> i16 {
    n.checked_abs().map_or(i16::MIN, |a| -a)
}

#[derive(Debug, Clone)]
pub struct CellNpc {
    pub player: bool,
    pub race: Race,
    pub gender: Gender,
    pub class: NpcClass
}

#[derive(Debug, Clone)]
pub enum CellObj {
    Door { locked: Option<bool> },
    Chest { locked: bool },
}

#[derive(Debug, Clone)]
pub enum Cell {
    Wall,
    Vis { obj: Option<CellObj>, npc: Option<CellNpc> },
    Invis { roof: bool },
}

#[derive(Debug)]
pub struct VisibleArea {
    bounds: Rect,
    cells: Vec<Cell>,
}

impl VisibleArea {
    pub fn size(visibility: i8) -> u8 {
        1 + 2 * max(0, visibility) as u8
    }

    pub fn new(center: Point, visibility: i8) -> Self {
        let size = Self::size(visibility);
        let cells = vec![Cell::Wall; size as usize * size as usize];
        let size = Vector { x: size as u16 as i16, y: size as u16 as i16 };
        let center_margin = Thickness::align(Vector { x: 1, y: 1 }, size, HAlign::Center, VAlign::Center);
        let bounds = center_margin.expand_rect(Rect { tl: center, size: Vector { x: 1, y: 1 } });
        VisibleArea { bounds, cells }
    }

    pub fn bounds(&self) -> Rect { self.bounds }
}

impl Index<Point> for VisibleArea {
    type Output = Cell;

    fn index(&self, index: Point) -> &Cell {
        assert!(self.bounds.contains(index));
        let v = index.offset_from(self.bounds.tl);
        let i = (v.y as u16 as usize) * (self.bounds.w() as u16 as usize) + v.x as u16 as usize;
        &self.cells[i]
    }
}

impl IndexMut<Point> for VisibleArea {
    fn index_mut(&mut self, index: Point) -> &mut Cell {
        assert!(self.bounds.contains(index));
        let v = index.offset_from(self.bounds.tl);
        let i = (v.y as u16 as usize) * (self.bounds.w() as u16 as usize) + v.x as u16 as usize;
        &mut self.cells[i]
    }
}

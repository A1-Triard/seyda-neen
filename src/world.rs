use alloc::boxed::Box;
use alloc::vec;
use alloc::vec::Vec;
use components_arena::{Arena, Component, Id, NewtypeComponentId};
use core::any::Any;
use core::cmp::max;
use core::mem::replace;
use core::ops::{Index, IndexMut};
use educe::Educe;
use either::{Either, Left, Right};
use enum_derive_2018::IterVariants;
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

macro_attr! {
    #[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Hash)]
    #[derive(IterVariants!(DirectionVariants))]
    pub enum Direction {
        N, S, W, E,
        NW, NE, SW, SE
    }
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

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Hash)]
pub enum DoorState<LockState> {
    Opened,
    Closed { locked: LockState }
}

#[derive(Debug)]
pub struct Door {
    pub state: DoorState<NonMaxU8>,
    pub key: u16,
}

#[derive(Debug)]
pub struct Chest {
    pub locked: NonMaxU8,
    pub key: u16,
}

#[derive(Debug)]
pub enum ObjData {
    Door(Door),
    Chest(Chest),
    Npc(Npc),
    Wall { outer: bool },
    Roof,
}

impl ObjData {
    fn into_rt(self) -> ObjRt {
        match self {
            ObjData::Door(d) => ObjRt::Door { d, was_closed: true },
            ObjData::Chest(d) => ObjRt::Chest(d),
            ObjData::Npc(d) => ObjRt::Npc(d),
            ObjData::Wall { outer } => ObjRt::Wall { outer },
            ObjData::Roof => ObjRt::Roof,
        }
    }
}

#[derive(Debug)]
enum ObjRt {
    Door { d: Door, was_closed: bool },
    Chest(Chest),
    Npc(Npc),
    Wall { outer: bool },
    Roof,
}

macro_attr! {
    #[derive(Debug, Component!)]
    struct ObjNode {
        data: ObjRt,
        bounds: Rect,
        group: Option<Group>,
    }
}

macro_attr! {
    #[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Hash)]
    #[derive(NewtypeComponentId!)]
    pub struct Obj(Id<ObjNode>);
}

macro_attr! {
    #[derive(Debug, Component!)]
    struct Sector {
        bounds: Rect,
        data: Either<[Id<Sector>; 4], Vec<Obj>>,
    }
}

impl Sector {
    fn split_if_needed(this: Id<Self>, sectors: &mut Arena<Sector>, objs: &mut Arena<ObjNode>) {
        let sector = &mut sectors[this];
        if sector.bounds.area() <= SECTOR_AREA_MIN { return; }
        let sector_objs = if let Right(objs) = sector.data.as_mut() {
            if objs.len() < SECTOR_OBJS_MAX { return; }
            replace(objs, Vec::new())
        } else {
            return;
        };
        let center_x = sector.bounds.l().wrapping_add((sector.bounds.w() as u16 / 2) as i16);
        let center_y = sector.bounds.t().wrapping_add((sector.bounds.h() as u16 / 2) as i16);
        debug_assert!(sector.bounds.contains(Point { x: center_x, y: center_y }));
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
                Point { x: center_x, y: sector.bounds.b() }
            ),
            Rect::from_tl_br(
                Point { x: center_x, y: center_y },
                Point { x: sector.bounds.r(), y: sector.bounds.b() }
            )
        ];
        debug_assert_eq!(subsectors.iter().map(|x| x.area()).sum::<u32>(), sector.bounds.area());
        let subsectors = subsectors.map(|bounds| {
            let mut subsector_objs = Vec::new();
            for &obj in &sector_objs {
                if !objs[obj.0].bounds.intersect(bounds).is_empty() {
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

macro_attr! {
    #[derive(Debug, Component!)]
    struct GroupNode {
        objs: Vec<Obj>,
    }
}

macro_attr! {
    #[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Hash)]
    #[derive(NewtypeComponentId!)]
    pub struct Group(Id<GroupNode>);
}

#[derive(Debug)]
pub struct World {
    rng: SmallRng,
    player: Obj,
    objs: Arena<ObjNode>,
    sectors: Arena<Sector>,
    area: [Id<Sector>; 4],
    groups: Arena<GroupNode>,
}

impl World {
    pub fn new(player: Npc) -> Self {
        let mut objs = Arena::new();
        let player = objs.insert(|id| (ObjNode {
            data: ObjRt::Npc(player),
            bounds: Rect { tl: Point { x: 0, y: 0 }, size: Vector { x: 1, y: 1 } },
            group: None,
        }, Obj(id)));
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
        World {
            rng: SmallRng::from_entropy(),
            player,
            objs,
            sectors,
            area: [tr, tl, bl, br],
            groups: Arena::new()
        }
    }

    pub fn player(&self) -> Point {
        self.objs[self.player.0].bounds.tl
    }

    pub fn player_data_mut<T: Any>(&mut self) -> &mut T {
        match &mut self.objs[self.player.0].data {
            ObjRt::Npc(Npc { ai_data, .. }) => ai_data.downcast_mut().unwrap(),
            _ => unreachable!()
        }
    }

    fn add_raw(&mut self, bounds: Rect, obj: Obj) {
        for p in bounds.points() {
            let (sector, sector_objs) = self.sector_mut(p);
            if !sector_objs.iter().any(|&x| x == obj) {
                sector_objs.push(obj);
                Sector::split_if_needed(sector, &mut self.sectors, &mut self.objs);
            }
        }
    }

    fn remove_raw(&mut self, bounds: Rect, obj: Obj) {
        for p in bounds.points() {
            let (_, sector_objs) = self.sector_mut(p);
            if let Some(index) = sector_objs.iter().position(|&x| x == obj) {
                sector_objs.swap_remove(index);
            }
        }
    }

    pub fn add_obj(&mut self, group: Option<Group>, bounds: Rect, data: ObjData) -> Obj {
        let obj = self.objs.insert(|id|
            (ObjNode { bounds, data: data.into_rt(), group }, Obj(id))
        );
        if let Some(group) = group {
            self.groups[group.0].objs.push(obj);
        }
        self.add_raw(bounds, obj);
        obj
    }

    pub fn add_group(&mut self) -> Group {
        self.groups.insert(|id|
            (GroupNode { objs: Vec::new() }, Group(id))
        )
    }

    pub fn remove_obj(&mut self, obj: Obj) {
        let obj_node = self.objs.remove(obj.0);
        if let Some(group) = obj_node.group {
            let objs = &mut self.groups[group.0].objs;
            let index = objs.iter().position(|&x| x == obj).unwrap();
            objs.swap_remove(index);
        }
        self.remove_raw(obj_node.bounds, obj);
    }

    pub fn remove_group(&mut self, group: Group) {
        let group_node = self.groups.remove(group.0);
        for obj in group_node.objs {
            let obj_node = self.objs.remove(obj.0);
            debug_assert_eq!(obj_node.group, Some(group));
            self.remove_raw(obj_node.bounds, obj);
        }
    }

    fn sector(&self, p: Point) -> (Id<Sector>, &Vec<Obj>) {
        let mut subsectors = &self.area;
        loop {
            let &sector = subsectors.into_iter().find(|&&x| self.sectors[x].bounds.contains(p)).unwrap();
            match self.sectors[sector].data.as_ref() {
                Right(x) => break (sector, x),
                Left(x) => subsectors = x
            }
        }
    }

    fn sector_mut(&mut self, p: Point) -> (Id<Sector>, &mut Vec<Obj>) {
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

    fn objs(&self, p: Point) -> impl Iterator<Item=(Obj, Option<Group>, &ObjRt)> {
        self.sector(p).1.iter().filter_map(move |&obj| {
            let obj_node = &self.objs[obj.0];
            if obj_node.bounds.contains(p) {
                let group = obj_node.group;
                Some((obj, group, &obj_node.data))
            } else {
                None
            }
        })
    }

    fn is_barrier(&self, p: Point) -> bool {
        let mut wall = false;
        let mut door_closed = None;
        for (_, _, obj_rt) in self.objs(p) {
            match obj_rt {
                ObjRt::Wall { .. } => wall = true,
                ObjRt::Door { d: Door { state, .. }, .. } => {
                    door_closed = Some(matches!(state, DoorState::Closed { .. }));
                    break;
                }
                _ => { },
            }
        }
        door_closed.unwrap_or(wall)
    }

    pub fn step(&mut self) {
        struct NpcMove {
            id: Obj,
            from: Point,
            to: Option<Point>,
            ai: &'static dyn Ai,
            wants_close_door: bool,
            movement_order: u16,
        }
        let mut npcs = self.objs.items().iter()
            .filter_map(|(id, obj)|
                if let ObjNode { data: ObjRt::Npc(npc), bounds, .. } = obj {
                    let movement_order = i8::MAX.wrapping_sub(npc.movement_priority) as u8;
                    let movement_order = ((movement_order as u16) << 8) | (self.rng.gen::<u8>() as u16);
                    Some(NpcMove {
                        id: Obj(id),
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
            let ai_data = if let ObjRt::Npc(npc) = &mut self.objs[npc.id.0].data {
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
                let mut wall = false;
                let mut door = None;
                for (obj, _, obj_rt) in self.objs(to) {
                    match obj_rt {
                        ObjRt::Wall { .. } => wall = true,
                        ObjRt::Door { .. } => {
                            door = Some(obj);
                            break;
                        }
                        _ => { },
                    }
                }
                let deny = if let Some(door) = door {
                    match &mut self.objs[door.0].data {
                        ObjRt::Door { d: door, .. } => {
                            let open = match door.state {
                                DoorState::Closed { locked } => Some(locked.get() == 0),
                                DoorState::Opened => None,
                            };
                            if open.unwrap_or(false) { door.state = DoorState::Opened; }
                            open.map_or(false, |x| !x)
                        },
                        _ => unreachable!(),
                    }
                } else {
                    wall
                };
                if deny { 
                    npc.to = None;
                }
            }
        }
        npcs.sort_by_key(|x| x.movement_order);
        for npc in npcs {
            if let Some(to) = npc.to {
                if self.objs(to).any(|(_, _, x)| matches!(x, ObjRt::Npc(_))) { continue; }
                if npc.wants_close_door {
                    let from_door = self.objs(npc.from)
                        .find(|(_, _, x)| matches!(x, ObjRt::Door { .. }))
                        .map(|x| x.0);
                    if let Some(from_door) = from_door {
                        match &mut self.objs[from_door.0].data {
                            ObjRt::Door { d: door, .. } => {
                                if matches!(door.state, DoorState::Opened) {
                                    door.state = DoorState::Closed {
                                        locked: unsafe { NonMaxU8::new_unchecked(0) }
                                    };
                                }
                            },
                            _ => unreachable!(),
                        }
                    }
                }
                let new_bounds = Rect { tl: to, size: Vector { x: 1, y: 1 } };
                let old_bounds = replace(&mut self.objs[npc.id.0].bounds, new_bounds);
                self.remove_raw(old_bounds, npc.id);
                self.add_raw(new_bounds, npc.id);
            }
        }
    }

    pub fn render(&mut self, area: &mut VisibleArea, force_show_roof: bool) {
        let player = self.player();
        let mut roof_group = None;
        for (_, group, obj_rt) in self.objs(player) {
            match obj_rt {
                ObjRt::Roof => {
                    roof_group = group;
                    break;
                },
                _ => { },
            }
        }
        for p in area.bounds().points() {
            let is_visible = self.is_visible_from(player, p);
            if is_visible {
                let mut door = None;
                for (obj, _, obj_rt) in self.objs(p) {
                    match obj_rt {
                        ObjRt::Door { .. } => {
                            door = Some(obj);
                            break;
                        }
                        _ => { }
                    }
                }
                if let Some(door) = door {
                    if let ObjRt::Door { d, was_closed } = &mut self.objs[door.0].data {
                        *was_closed = matches!(d.state, DoorState::Closed { .. });
                    } else {
                        unreachable!();
                    }
                }
            }
            let mut wall_outer = None;
            let mut door_state_was_closed = None;
            let mut roof = false;
            let mut obj = None;
            let mut npc = None;
            for (_, group, obj_rt) in self.objs(p) {
                match obj_rt {
                    ObjRt::Roof => if group != roof_group { roof = true; },
                    &ObjRt::Wall { outer } => wall_outer = Some(outer),
                    &ObjRt::Door { ref d, was_closed } => door_state_was_closed = Some((
                        match d.state {
                            DoorState::Opened => DoorState::Opened,
                            DoorState::Closed { locked } =>
                                DoorState::Closed { locked: locked.get() != 0 },
                        },
                        was_closed
                    )),
                    ObjRt::Chest(chest) => obj = Some(CellObj::Chest {
                        locked: chest.locked.get() != 0
                    }),
                    ObjRt::Npc(npc_rt) => npc = Some(CellNpc {
                        player: p == player,
                        race: npc_rt.race,
                        gender: npc_rt.gender,
                        class: npc_rt.class
                    }),
                }
            }
            area[p] = if roof && (force_show_roof || (!is_visible && !wall_outer.unwrap_or(false))) {
                Cell::Roof(if door_state_was_closed.is_some() {
                    CellRoof::Door
                } else if wall_outer.is_some() {
                    CellRoof::Wall
                } else {
                    CellRoof::None
                })
            } else if is_visible && (wall_outer.is_none() || door_state_was_closed.is_some()) {
                let door = door_state_was_closed.map(|x| x.0);
                let obj = npc.map(CellObj::Npc).or(obj);
                Cell::Vis { obj, door }
            } else {
                if let Some((_, door_was_closed)) = door_state_was_closed {
                    Cell::InvisDoor { closed: door_was_closed }
                } else if let Some(wall_outer) = wall_outer {
                    Cell::Wall { outer: wall_outer }
                } else {
                    Cell::None
                }
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
    Chest { locked: bool },
    Npc(CellNpc),
}

#[derive(Debug, Clone)]
pub enum CellRoof {
    None, Door, Wall
}

#[derive(Debug, Clone)]
pub enum Cell {
    None,
    Roof(CellRoof),
    InvisDoor { closed: bool },
    Wall { outer: bool },
    Vis { obj: Option<CellObj>, door: Option<DoorState<bool>> },
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
        let cells = vec![Cell::None; size as usize * size as usize];
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

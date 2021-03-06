use alloc::boxed::Box;
use alloc::vec;
use alloc::vec::Vec;
use components_arena::{Arena, Component, Id, NewtypeComponentId};
use core::any::Any;
use core::cmp::max;
use core::fmt::{self, Display, Formatter};
use core::num::NonZeroU8;
use core::ops::{Index, IndexMut};
use educe::Educe;
use enum_derive_2018::IterVariants;
use macro_attr_2018::macro_attr;
use nonmax::NonMaxU8;
use rand::{Rng, SeedableRng};
use rand::rngs::SmallRng;
use tuifw_screen::{HAlign, VAlign, Point, Rect, Thickness, Vector};
use crate::space::{self, Space};

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Hash)]
pub enum Language { En, Ru }

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Hash)]
pub enum GrammarGender {
    Male,
    Female,
    Neutral
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Hash)]
pub enum Raw {
    Iron,
    Silver,
    Glass,
    Ebony,
    Gold,
}

#[derive(Debug, Clone)]
pub enum MetalGrammar {
    Ru { gender: GrammarGender },
    En,
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Hash)]
pub enum Metal {
    Chitin,
    Steel,
    Silver,
    Glass,
    Ebony,
    Daedric,
}

impl Metal {
    pub fn weight_x50(self) -> u8 {
        match self {
            Metal::Chitin => 25,
            Metal::Steel => 50,
            Metal::Silver => 40,
            Metal::Glass => 30,
            Metal::Ebony => 100,
            Metal::Daedric => 150,
        }
    }

    pub fn display(self, low_quality: bool, grammar: &MetalGrammar) -> &'static str {
        match (low_quality, self, grammar) {
            (_, Metal::Chitin, MetalGrammar::En) => "Chitin",
            (_, Metal::Chitin, MetalGrammar::Ru {
                gender: GrammarGender::Male
            }) => "Хитиновый",
            (_, Metal::Chitin, MetalGrammar::Ru {
                gender: GrammarGender::Female
            }) => "Хитиновая",
            (_, Metal::Chitin, MetalGrammar::Ru {
                gender: GrammarGender::Neutral
            }) => "Хитиновое",
            (false, Metal::Steel, MetalGrammar::En) => "Steel",
            (false, Metal::Steel, MetalGrammar::Ru {
                gender: GrammarGender::Male
            }) => "Стальной",
            (false, Metal::Steel, MetalGrammar::Ru {
                gender: GrammarGender::Female
            }) => "Стальная",
            (false, Metal::Steel, MetalGrammar::Ru {
                gender: GrammarGender::Neutral
            }) => "Стальное",
            (true, Metal::Steel, MetalGrammar::En) => "Iron",
            (true, Metal::Steel, MetalGrammar::Ru {
                gender: GrammarGender::Male
            }) => "Железный",
            (true, Metal::Steel, MetalGrammar::Ru {
                gender: GrammarGender::Female
            }) => "Железная",
            (true, Metal::Steel, MetalGrammar::Ru {
                gender: GrammarGender::Neutral
            }) => "Железное",
            (_, Metal::Silver, MetalGrammar::En) => "Silver",
            (_, Metal::Silver, MetalGrammar::Ru {
                gender: GrammarGender::Male
            }) => "Серебряный",
            (_, Metal::Silver, MetalGrammar::Ru {
                gender: GrammarGender::Female
            }) => "Серебряная",
            (_, Metal::Silver, MetalGrammar::Ru {
                gender: GrammarGender::Neutral
            }) => "Серебряное",
            (_, Metal::Glass, MetalGrammar::En) => "Glass",
            (_, Metal::Glass, MetalGrammar::Ru {
                gender: GrammarGender::Male
            }) => "Стеклянный",
            (_, Metal::Glass, MetalGrammar::Ru {
                gender: GrammarGender::Female
            }) => "Стеклянная",
            (_, Metal::Glass, MetalGrammar::Ru {
                gender: GrammarGender::Neutral
            }) => "Стеклянное",
            (_, Metal::Ebony, MetalGrammar::En) => "Ebony",
            (_, Metal::Ebony, MetalGrammar::Ru {
                gender: GrammarGender::Male
            }) => "Эбонитовый",
            (_, Metal::Ebony, MetalGrammar::Ru {
                gender: GrammarGender::Female
            }) => "Эбонитовая",
            (_, Metal::Ebony, MetalGrammar::Ru {
                gender: GrammarGender::Neutral
            }) => "Эбонитовое",
            (_, Metal::Daedric, MetalGrammar::En) => "Daedric",
            (_, Metal::Daedric, MetalGrammar::Ru {
                gender: GrammarGender::Male
            }) => "Даэдрический",
            (_, Metal::Daedric, MetalGrammar::Ru {
                gender: GrammarGender::Female
            }) => "Даэдрическая",
            (_, Metal::Daedric, MetalGrammar::Ru {
                gender: GrammarGender::Neutral
            }) => "Даэдрическое",
        }
    }
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Hash)]
pub enum WeaponDesignOrigin {
    Elven,
    Akaviri,
    Dwarven,
    Orcish,
    Nordic,
    Imperial,
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Hash)]
pub enum BladeType {
    Dagger,
    Short,
    Long,
    TwoHanded,
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Hash)]
pub struct BladeDesign {
    pub origin: WeaponDesignOrigin,
    pub ty: BladeType,
}

impl BladeDesign {
    pub fn weight_x2(self) -> u8 {
        match self {
            BladeDesign { origin: WeaponDesignOrigin::Elven, ty: BladeType::Dagger } => 6,
            BladeDesign { origin: WeaponDesignOrigin::Akaviri, ty: BladeType::Dagger } => 8,
            BladeDesign { origin: WeaponDesignOrigin::Dwarven, ty: BladeType::Dagger } => 6,
            BladeDesign { origin: WeaponDesignOrigin::Orcish, ty: BladeType::Dagger } => 7,
            BladeDesign { origin: WeaponDesignOrigin::Nordic, ty: BladeType::Dagger } => 8,
            BladeDesign { origin: WeaponDesignOrigin::Imperial, ty: BladeType::Dagger } => 7,
            BladeDesign { origin: WeaponDesignOrigin::Elven, ty: BladeType::Short } => 16,
            BladeDesign { origin: WeaponDesignOrigin::Akaviri, ty: BladeType::Short } => 20,
            BladeDesign { origin: WeaponDesignOrigin::Dwarven, ty: BladeType::Short } => 16,
            BladeDesign { origin: WeaponDesignOrigin::Orcish, ty: BladeType::Short } => 18,
            BladeDesign { origin: WeaponDesignOrigin::Nordic, ty: BladeType::Short } => 20,
            BladeDesign { origin: WeaponDesignOrigin::Imperial, ty: BladeType::Short } => 18,
            BladeDesign { origin: WeaponDesignOrigin::Elven, ty: BladeType::Long } => 40,
            BladeDesign { origin: WeaponDesignOrigin::Akaviri, ty: BladeType::Long } => 36,
            BladeDesign { origin: WeaponDesignOrigin::Dwarven, ty: BladeType::Long } => 40,
            BladeDesign { origin: WeaponDesignOrigin::Orcish, ty: BladeType::Long } => 24,
            BladeDesign { origin: WeaponDesignOrigin::Nordic, ty: BladeType::Long } => 36,
            BladeDesign { origin: WeaponDesignOrigin::Imperial, ty: BladeType::Long } => 24,
            BladeDesign { origin: WeaponDesignOrigin::Elven, ty: BladeType::TwoHanded } => 54,
            BladeDesign { origin: WeaponDesignOrigin::Akaviri, ty: BladeType::TwoHanded } => 40,
            BladeDesign { origin: WeaponDesignOrigin::Dwarven, ty: BladeType::TwoHanded } => 54,
            BladeDesign { origin: WeaponDesignOrigin::Orcish, ty: BladeType::TwoHanded } => 30,
            BladeDesign { origin: WeaponDesignOrigin::Nordic, ty: BladeType::TwoHanded } => 60,
            BladeDesign { origin: WeaponDesignOrigin::Imperial, ty: BladeType::TwoHanded } => 36,
        }
    }

    pub fn name(self, lang: Language) -> (MetalGrammar, &'static str) {
        match lang {
            Language::En => (
                MetalGrammar::En,
                match self {
                    BladeDesign { ty: BladeType::Dagger, origin: WeaponDesignOrigin::Akaviri } =>
                        "Tanto",
                    BladeDesign { ty: BladeType::Dagger, .. } =>
                        "Dagger",
                    BladeDesign { ty: BladeType::Short, origin: WeaponDesignOrigin::Akaviri } =>
                        "Wakizashi",
                    BladeDesign { ty: BladeType::Short, .. } =>
                        "Short Sword",
                    BladeDesign { ty: BladeType::Long, origin: WeaponDesignOrigin::Akaviri } =>
                        "Katana",
                    BladeDesign { ty: BladeType::Long, origin: WeaponDesignOrigin::Orcish } |
                    BladeDesign { ty: BladeType::Long, origin: WeaponDesignOrigin::Nordic } |
                    BladeDesign { ty: BladeType::Long, origin: WeaponDesignOrigin::Imperial } =>
                        "Broad Sword",
                    BladeDesign { ty: BladeType::Long, .. } =>
                        "Long Sword",
                    BladeDesign { ty: BladeType::TwoHanded, origin: WeaponDesignOrigin::Akaviri } =>
                        "Dai-katana",
                    BladeDesign { ty: BladeType::TwoHanded, .. } =>
                        "Claymore",
                }
            ),
            Language::Ru => {
                let (gender, name) = match self {
                    BladeDesign { ty: BladeType::Dagger, origin: WeaponDesignOrigin::Akaviri } =>
                        (GrammarGender::Male, "танто"),
                    BladeDesign { ty: BladeType::Dagger, .. } =>
                        (GrammarGender::Male, "кинжал"),
                    BladeDesign { ty: BladeType::Short, origin: WeaponDesignOrigin::Akaviri } =>
                        (GrammarGender::Male, "вакидзаси"),
                    BladeDesign { ty: BladeType::Short, .. } =>
                        (GrammarGender::Male, "короткий меч"),
                    BladeDesign { ty: BladeType::Long, origin: WeaponDesignOrigin::Akaviri } =>
                        (GrammarGender::Female, "катана"),
                    BladeDesign { ty: BladeType::Long, origin: WeaponDesignOrigin::Orcish } |
                    BladeDesign { ty: BladeType::Long, origin: WeaponDesignOrigin::Nordic } |
                    BladeDesign { ty: BladeType::Long, origin: WeaponDesignOrigin::Imperial } =>
                        (GrammarGender::Male, "палаш"),
                    BladeDesign { ty: BladeType::Long, .. } =>
                        (GrammarGender::Male, "длинный меч"),
                    BladeDesign { ty: BladeType::TwoHanded, origin: WeaponDesignOrigin::Akaviri } =>
                        (GrammarGender::Female, "дайкатана"),
                    BladeDesign { ty: BladeType::TwoHanded, .. } =>
                        (GrammarGender::Female, "клеймора"),
                };
                (MetalGrammar::Ru { gender }, name)
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct Blade {
    pub design: BladeDesign,
    pub material: Metal,
    pub low_quality: bool,
}

struct BladeName<'a>(&'a Blade, Language);

impl<'a> Display for BladeName<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let (material_grammar, name) = self.0.design.name(self.1);
        let material = self.0.material.display(self.0.low_quality, &material_grammar);
        write!(f, "{} {}", material, name)
    }
}

impl Blade {
    pub fn name(&self, lang: Language) -> impl Display + '_ {
        BladeName(self, lang)
    }

    pub fn weight_x100(&self) -> u16 {
        self.design.weight_x2() as u16 * self.material.weight_x50() as u16
    }
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Hash)]
pub enum Herb {
    BanglersBane,
}

#[derive(Debug, Clone)]
pub enum Item {
    Herb(Herb),
    Blade(Blade),
    Raw(Raw),
}

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
    Herb(NonZeroU8, Herb),
    Item(Item),
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
            ObjData::Herb(n, t) => ObjRt::Herb(n, t, n.get()),
            ObjData::Item(d) => ObjRt::Item(d),
            ObjData::Npc(d) => ObjRt::Npc(d),
            ObjData::Wall { outer } => ObjRt::Wall { outer },
            ObjData::Roof => ObjRt::Roof,
        }
    }
}

#[derive(Debug)]
enum ObjRt {
    Item(Item),
    Herb(NonZeroU8, Herb, u8),
    Door { d: Door, was_closed: bool },
    Chest(Chest),
    Npc(Npc),
    Wall { outer: bool },
    Roof,
}

#[derive(Debug)]
pub struct ObjNode {
    data: ObjRt,
    group: Option<Group>,
}

pub type Obj = space::Obj<ObjNode>;

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
    space: Space<ObjNode>,
    groups: Arena<GroupNode>,
}

impl World {
    pub fn new(player: Npc) -> Self {
        let mut space = Space::new();
        let player = space.add(
            0,
            Rect { tl: Point { x: 0, y: 0 }, size: Vector { x: 1, y: 1 } },
            ObjNode {
                data: ObjRt::Npc(player),
                group: None,
            }
        );
        World {
            rng: SmallRng::from_entropy(),
            player,
            space,
            groups: Arena::new()
        }
    }

    pub fn player(&self) -> Point {
        self.space.bounds(self.player).tl
    }

    pub fn player_data_mut<T: Any>(&mut self) -> &mut T {
        match &mut self.space.data_mut(self.player).data {
            ObjRt::Npc(Npc { ai_data, .. }) => ai_data.downcast_mut().unwrap(),
            _ => unreachable!()
        }
    }

    pub fn add_obj(&mut self, group: Option<Group>, bounds: Rect, data: ObjData) -> Obj {
        let obj = self.space.add(0, bounds, ObjNode {
            data: data.into_rt(),
            group
        });
        if let Some(group) = group {
            self.groups[group.0].objs.push(obj);
        }
        obj
    }

    pub fn add_group(&mut self) -> Group {
        self.groups.insert(|id|
            (GroupNode { objs: Vec::new() }, Group(id))
        )
    }

    pub fn remove_obj(&mut self, obj: Obj) {
        let group = self.space.data(obj).group;
        if let Some(group) = group {
            let objs = &mut self.groups[group.0].objs;
            let index = objs.iter().position(|&x| x == obj).unwrap();
            objs.swap_remove(index);
        }
        self.space.remove(obj);
    }

    pub fn remove_group(&mut self, group: Group) {
        let group_node = self.groups.remove(group.0);
        for obj in group_node.objs {
            self.space.remove(obj);
        }
    }

    fn is_barrier(&self, p: Point) -> bool {
        let mut wall = false;
        let mut door_closed = None;
        for obj_rt in self.space.data_at(0, p).map(|x| &x.data) {
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
        let mut npcs = self.space.objs_bounds()
            .filter_map(|(id, obj, plane, bounds)| {
                debug_assert_eq!(plane, 0);
                if let ObjNode { data: ObjRt::Npc(npc), .. } = obj {
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
            })
            .collect::<Vec<_>>();
        for npc in &mut npcs {
            let ai_data = if let ObjRt::Npc(npc) = &mut self.space.data_mut(npc.id).data {
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
                for (obj, obj_node) in self.space.objs_data_at(0, to) {
                    match obj_node.data {
                        ObjRt::Wall { .. } => wall = true,
                        ObjRt::Door { .. } => {
                            door = Some(obj);
                            break;
                        }
                        _ => { },
                    }
                }
                let deny = if let Some(door) = door {
                    match &mut self.space.data_mut(door).data {
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
                if self.space.data_at(0, to).any(|x| matches!(x.data, ObjRt::Npc(_))) { continue; }
                if npc.wants_close_door {
                    let from_door = self.space.objs_data_at(0, npc.from)
                        .find(|(_, x)| matches!(x.data, ObjRt::Door { .. }))
                        .map(|x| x.0);
                    if let Some(from_door) = from_door {
                        match &mut self.space.data_mut(from_door).data {
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
                self.space.move_(npc.id, 0, Rect { tl: to, size: Vector { x: 1, y: 1 } });
            }
        }
    }

    pub fn render(&mut self, area: &mut VisibleArea, force_show_roof: bool) {
        let player = self.player();
        let mut roof_group = None;
        for obj_node in self.space.data_at(0, player) {
            match obj_node.data {
                ObjRt::Roof => {
                    roof_group = obj_node.group;
                    break;
                },
                _ => { },
            }
        }
        for p in area.bounds().points() {
            let is_visible = self.is_visible_from(player, p);
            if is_visible {
                let mut door = None;
                for (obj, obj_node) in self.space.objs_data_at(0, p) {
                    match &obj_node.data {
                        ObjRt::Door { .. } => {
                            door = Some(obj);
                            break;
                        }
                        _ => { }
                    }
                }
                if let Some(door) = door {
                    if let ObjRt::Door { d, was_closed } = &mut self.space.data_mut(door).data {
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
            for obj_node in self.space.data_at(0, p) {
                match &obj_node.data {
                    ObjRt::Roof => if obj_node.group != roof_group { roof = true; },
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
                    ObjRt::Item(d) => obj = Some(CellObj::Item(d.clone())),
                    ObjRt::Herb(_, _, 0) => { },
                    &ObjRt::Herb(_, d, _) => obj = Some(CellObj::Herb(d)),
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
    Herb(Herb),
    Item(Item),
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

use alloc::collections::BTreeMap;
use alloc::vec::Vec;
use components_arena::{Arena, Component, Id, NewtypeComponentId};
use core::iter::FusedIterator;
use core::mem::replace;
use educe::Educe;
use either::{Either, Left, Right};
use int_vec_2d::{Point, Rect};
use macro_attr_2018::macro_attr;

const SECTOR_OBJS_MAX: usize = 32;
const SECTOR_AREA_MIN: u32 = 16;

macro_attr! {
    #[derive(Debug, Component!(class=ObjClass))]
    struct ObjNode<T> {
        data: T,
        plane: i16,
        bounds: Rect,
    }
}

macro_attr! {
    #[derive(Educe, NewtypeComponentId!)]
    #[educe(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Hash)]
    pub struct Obj<T>(Id<ObjNode<T>>);
}

macro_attr! {
    #[derive(Debug, Component!(class=SectorClass))]
    struct Sector<T> {
        bounds: Rect,
        data: Either<[Id<Sector<T>>; 4], Vec<Obj<T>>>,
    }
}

impl<T> Sector<T> {
    fn split_if_needed(this: Id<Self>, sectors: &mut Arena<Sector<T>>, objs: &mut Arena<ObjNode<T>>) {
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

#[derive(Debug)]
struct Plane<T: 'static> {
    sectors: Arena<Sector<T>>,
    area: [Id<Sector<T>>; 4],
}

impl<T> Plane<T> {
    fn new() -> Self {
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
                Point { x: i16::MIN, y: 0 }
            ),
            data: Right(Vec::new())
        }, id));
        let bl = sectors.insert(|id| (Sector {
            bounds: Rect::from_tl_br(
                Point { x: i16::MIN, y: 0 },
                Point { x: 0, y: i16::MIN }
            ),
            data: Right(Vec::new())
        }, id));
        let br = sectors.insert(|id| (Sector {
            bounds: Rect::from_tl_br(
                Point { x: 0, y: 0 },
                Point { x: i16::MIN, y: i16::MIN }
            ),
            data: Right(Vec::new())
        }, id));
        Plane {
            sectors,
            area: [tr, tl, bl, br],
        }
    }

    fn sector(&self, p: Point) -> (Id<Sector<T>>, &Vec<Obj<T>>) {
        let mut subsectors = &self.area;
        loop {
            let &sector = subsectors.into_iter().find(|&&x| self.sectors[x].bounds.contains(p)).unwrap();
            match self.sectors[sector].data.as_ref() {
                Right(x) => break (sector, x),
                Left(x) => subsectors = x
            }
        }
    }

    fn sector_mut(&mut self, p: Point) -> (Id<Sector<T>>, &mut Vec<Obj<T>>) {
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
}

#[derive(Debug)]
pub struct Space<T: 'static> {
    objs: Arena<ObjNode<T>>,
    planes: BTreeMap<i16, Plane<T>>,
}

impl<T> Space<T> {
    pub const fn new() -> Self {
        Space {
            objs: Arena::new(),
            planes: BTreeMap::new()
        }
    }

    fn add_raw(&mut self, plane: i16, bounds: Rect, obj: Obj<T>) {
        let plane = self.planes.entry(plane).or_insert(Plane::new());
        for p in bounds.points() {
            let (sector, sector_objs) = plane.sector_mut(p);
            if !sector_objs.iter().any(|&x| x == obj) {
                sector_objs.push(obj);
                Sector::split_if_needed(sector, &mut plane.sectors, &mut self.objs);
            }
        }
    }

    fn remove_raw(&mut self, plane: i16, bounds: Rect, obj: Obj<T>) {
        let plane = self.planes.get_mut(&plane).unwrap();
        for p in bounds.points() {
            let (_, sector_objs) = plane.sector_mut(p);
            if let Some(index) = sector_objs.iter().position(|&x| x == obj) {
                sector_objs.swap_remove(index);
            }
        }
    }

    pub fn add(&mut self, plane: i16, bounds: Rect, data: T) -> Obj<T> {
        let obj = self.objs.insert(|id|
            (ObjNode { plane, bounds, data }, Obj(id))
        );
        self.add_raw(plane, bounds, obj);
        obj
    }

    pub fn remove(&mut self, obj: Obj<T>) {
        let obj_node = self.objs.remove(obj.0);
        self.remove_raw(obj_node.plane, obj_node.bounds, obj);
    }

    pub fn move_(&mut self, obj: Obj<T>, plane: i16, bounds: Rect) {
        let obj_node = &self.objs[obj.0];
        let old_plane = obj_node.plane;
        let old_bounds = obj_node.bounds;
        self.remove_raw(old_plane, old_bounds, obj);
        self.add_raw(plane, bounds, obj);
    }

    pub fn objs(
        &self,
        plane: i16, p: Point
    ) -> impl Iterator<Item=(Obj<T>, &T)> + DoubleEndedIterator + FusedIterator {
        self.planes.get(&plane).into_iter()
            .flat_map(move |plane| plane.sector(p).1.iter())
            .filter_map(move |&obj| {
                let obj_node = &self.objs[obj.0];
                debug_assert_eq!(obj_node.plane, plane);
                if obj_node.bounds.contains(p) {
                    Some((obj, &obj_node.data))
                } else {
                    None
                }
            })
    }
}

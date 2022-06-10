use components_arena::{Arena, Component, Id};
use either::{Either, Left, Right};
use macro_attr_2018::macro_attr;
use std::cmp::max;
use std::mem::replace;
use std::num::NonZeroU8;
use std::ops::{Index, IndexMut};
use tuifw_screen::{HAlign, VAlign, Point, Rect, Thickness, Vector};

const BUILDINGS_PER_SECTOR_MAX: usize = 16;
const SECTOR_AREA_MIN: u32 = 16;

macro_attr! {
    #[derive(Debug, Component!)]
    pub struct Sector {
        rect: Rect,
        data: Either<[Id<Sector>; 4], Vec<Id<Building>>>,
    }
}

impl Sector {
    fn split_if_needed(this: Id<Self>, sectors: &mut Arena<Sector>, buildings: &mut Arena<Building>) {
        let sector = &mut sectors[this];
        if sector.rect.area() <= SECTOR_AREA_MIN { return; }
        let sector_buildings = if let Right(buildings) = sector.data.as_mut() {
            if buildings.len() < BUILDINGS_PER_SECTOR_MAX { return; }
            replace(buildings, Vec::new())
        } else {
            return;
        };
        let center_x = sector.rect.l() + sector.rect.w() / 2;
        let center_y = sector.rect.t() + sector.rect.h() / 2;
        let subsectors = [
            Rect::from_tl_br(
                Point { x: center_x, y: sector.rect.t() },
                Point { x: sector.rect.r(), y: center_y }
            ),
            Rect::from_tl_br(
                Point { x: sector.rect.l(), y: sector.rect.t() },
                Point { x: center_x, y: center_y }
            ),
            Rect::from_tl_br(
                Point { x: sector.rect.l(), y: center_y },
                Point { x: sector.rect.l(), y: sector.rect.b() }
            ),
            Rect::from_tl_br(
                Point { x: center_x, y: center_y },
                Point { x: sector.rect.r(), y: sector.rect.b() }
            )
        ].map(|rect| sectors.insert(|id| (Sector { rect, data: Right(Vec::new()) }, id)));
        sectors[this].data = Left(subsectors);
        for subsector in subsectors {
            for &building in &sector_buildings {
                let subsector = &mut sectors[subsector];
                if !buildings[building].rect().intersect(subsector.rect).is_empty() {
                    subsector.data.as_mut().right().unwrap().push(building);
                }
            }
            Self::split_if_needed(subsector, sectors, buildings);
        }
    }
}

macro_attr! {
    #[derive(Debug, Component!)]
    struct Building {
        tl: Point,
        w: NonZeroU8,
        h: NonZeroU8,
        door: u16,
    }
}

impl Building {
    fn rect(&self) -> Rect {
        Rect { tl: self.tl, size: Vector {
            x: self.w.get() as u16 as i16,
            y: self.h.get() as u16 as i16
        } }
    }

    fn door(&self) -> Point {
        self.tl.offset(self.door_offset())
    }

    fn door_offset(&self) -> Vector {
        if self.door < self.h.get() as u16 {
            Vector {
                x: 0,
                y: self.door as i16
            }
        } else if self.door < self.h.get() as u16 + self.w.get() as u16 {
            Vector {
                x: (self.door - self.h.get() as u16) as i16,
                y: (self.h.get() - 1) as u16 as i16
            }
        } else if self.door < 2 * self.h.get() as u16 + self.w.get() as u16 {
            Vector {
                x: (self.w.get() - 1) as u16 as i16, 
                y: (
                    (self.h.get() - 1) as u16 -
                    (self.door - self.h.get() as u16 - self.w.get() as u16)
                ) as i16
            }
        } else {
            Vector {
                x: (
                    (self.w.get() - 1) as u16 -
                    (self.door - 2 * self.h.get() as u16 - self.w.get() as u16)
                ) as i16,
                y: 0
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy, Hash)]
pub enum Cell {
    None,
    Door,
    Player,
    Wall,
}

#[derive(Debug)]
pub struct World {
    player: Point,
    buildings: Arena<Building>,
    sectors: Arena<Sector>,
    area: Id<Sector>,
}

impl World {
    pub fn new() -> Self {
        let mut sectors = Arena::new();
        let area = sectors.insert(|id| (Sector {
            rect: Rect {
                tl: Point { x: i16::MIN, y: i16::MIN },
                size: Vector { x: -1, y: -1 }
            },
            data: Right(Vec::new())
        }, id));
        World {
            player: Point { x: 0, y: 0 },
            buildings: Arena::new(),
            sectors,
            area
        }
    }

    pub fn player(&self) -> Point {
        self.player
    }

    pub fn move_player(&mut self, p: Point) {
        if self.cell(p) != Cell::Wall {
            self.player = p;
        }
    }

    fn sector(&self, p: Point) -> Option<(Id<Sector>, &Vec<Id<Building>>)> {
        let mut sector = self.area;
        if !self.sectors[sector].rect.contains(p) { return None; }
        Some(loop {
            match self.sectors[sector].data.as_ref() {
                Right(x) => break (sector, x),
                Left(subsectors) => {
                    sector = subsectors.into_iter().copied().find(|&x| self.sectors[x].rect.contains(p)).unwrap();
                }
            }
        })
    }

    fn sector_mut(&mut self, p: Point) -> Option<(Id<Sector>, &mut Vec<Id<Building>>)> {
        let mut sector = self.area;
        if !self.sectors[sector].rect.contains(p) { return None; }
        let (sector, buildings) = loop {
            let subsectors = match self.sectors[sector].data.as_mut() {
                Right(x) => break (sector, x as *mut _),
                Left(subsectors) => subsectors.clone()
            };
            sector = subsectors.into_iter().find(|&x| self.sectors[x].rect.contains(p)).unwrap();
        };
        Some((sector, unsafe { &mut *buildings }))
    }

    pub fn add_building(&mut self, tl: Point, w: NonZeroU8, h: NonZeroU8, door: u16) {
        let (building, rect) = self.buildings.insert(|id| {
            let building = Building {
                tl, w, h,
                door: door % (2 * w.get() as u16 + 2 * h.get() as u16)
            };
            let rect = building.rect();
            (building, (id, rect))
        });
        for p in rect.points() {
            let (sector, sector_buildings) = self.sector_mut(p).unwrap();
            if !sector_buildings.iter().any(|&x| x == building) {
                sector_buildings.push(building);
                Sector::split_if_needed(sector, &mut self.sectors, &mut self.buildings);
            }
        }
    }

    fn cell(&self, p: Point) -> Cell {
        if let Some((_, sector_buildings)) = self.sector(p) {
            for &building in sector_buildings {
                let rect = self.buildings[building].rect();
                if
                    rect.l_line().contains(p) || rect.t_line().contains(p) ||
                    rect.r_line().contains(p) || rect.b_line().contains(p)
                {
                    let door = self.buildings[building].door() == p;
                    return if door { Cell::Door } else { Cell::Wall };
                }
            }
            Cell::None
        } else {
            Cell::None
        }
    }

    pub fn render(&self, area: &mut VisibleArea) {
        for p in area.bounds().points() {
            area[p] = self.cell(p);
        }
        if area.bounds().contains(self.player) {
            area[self.player] = Cell::Player;
        }
    }
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

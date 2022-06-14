use nonmax::NonMaxU8;
use tuifw_screen::{Point, Rect, Vector};
use crate::world::*;

fn h_wall(x_min: i16, x_max: i16, y: i16, world: &mut World) {
    world.add_obj(
        None,
        Rect {
            tl: Point { x: x_min, y },
            size: Vector { x: x_max.wrapping_sub(x_min).wrapping_add(1), y: 1 }
        },
        ObjData::Wall
    );
}

fn v_wall(x: i16, y_min: i16, y_max: i16, world: &mut World) {
    world.add_obj(
        None,
        Rect {
            tl: Point { x, y: y_min },
            size: Vector { x: 1, y: y_max.wrapping_sub(y_min).wrapping_add(1) }
        },
        ObjData::Wall
    );
}

fn door(x: i16, y: i16, world: &mut World) {
    world.add_obj(
        None,
        Rect {
            tl: Point { x, y },
            size: Vector { x: 1, y: 1 }
        },
        ObjData::Door(Door {
            locked: Some(unsafe { NonMaxU8::new_unchecked(0) }),
            key: 0
        })
    );
}

fn roof(x_min: i16, x_max: i16, y_min: i16, y_max: i16, group: Group, world: &mut World) {
    world.add_obj(
        Some(group),
        Rect::from_tl_br(
            Point { x: x_min, y: y_min },
            Point { x: x_max.wrapping_add(1), y: y_max.wrapping_add(1) }
        ),
        ObjData::Roof
    );
}

pub fn add_imperial_office(world: &mut World) {
    let roof_group = world.add_group();
    h_wall(-2, 5, 12, world);
    h_wall(-2, 5, 18, world);
    v_wall(5, 13, 17, world);
    door(3, 18, world);
    v_wall(-2, 13, 17, world);
    door(-2, 14, world);
    h_wall(-6, -3, 13, world);
    h_wall(-8, -3, 15, world);
    v_wall(-4, 9, 13, world);
    h_wall(-9, -4, 9, world);
    v_wall(-9, 9, 15, world);
    door(-7, 9, world);
    roof(-1, 4, 13, 17, roof_group, world);
    roof(-4, 0, 14, 14, roof_group, world);
    roof(-8, -5, 10, 13, roof_group, world);
}

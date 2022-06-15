use nonmax::NonMaxU8;
use tuifw_screen::{Point, Rect, Vector};
use crate::world::*;

fn h_wall(x_min: i16, x_max: i16, y: i16, outer: bool, world: &mut World) {
    world.add_obj(
        None,
        Rect {
            tl: Point { x: x_min, y },
            size: Vector { x: x_max.wrapping_sub(x_min).wrapping_add(1), y: 1 }
        },
        ObjData::Wall { outer }
    );
}

fn v_wall(x: i16, y_min: i16, y_max: i16, outer: bool, world: &mut World) {
    world.add_obj(
        None,
        Rect {
            tl: Point { x, y: y_min },
            size: Vector { x: 1, y: y_max.wrapping_sub(y_min).wrapping_add(1) }
        },
        ObjData::Wall { outer }
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
            state: DoorState::Closed { locked: unsafe { NonMaxU8::new_unchecked(0) } },
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
    h_wall(-2, 5, 22, true, world);
    h_wall(-2, 5, 28, true, world);
    v_wall(5, 23, 27, true, world);
    door(3, 28, world);
    v_wall(-2, 23, 23, true, world);
    v_wall(-2, 25, 27, true, world);
    door(-2, 24, world);
    h_wall(-6, -5, 23, false, world);
    h_wall(-4, -3, 23, true, world);
    h_wall(-8, -3, 25, true, world);
    v_wall(-4, 19, 23, true, world);
    h_wall(-9, -4, 19, true, world);
    v_wall(-9, 19, 25, true, world);
    door(-8, 19, world);
    roof(-2, 5, 22, 28, roof_group, world);
    roof(-4, -3, 23, 25, roof_group, world);
    roof(-9, -4, 19, 25, roof_group, world);
    h_wall(-11, -10, 20, true, world);
    v_wall(-11, 14, 19, true, world);
    h_wall(-10, -7, 14, true, world);
    let counselor = world.add_group();
    h_wall(-6, -1, 13, true, world);
    h_wall(-6, -1, 17, true, world);
    v_wall(-1, 14, 16, true, world);
    v_wall(-6, 14, 16, true, world);
    door(-6, 16, world);
    v_wall(-5, 18, 18, true, world);
    door(-1, 16, world);
    roof(-6, -1, 13, 17, counselor, world);
}

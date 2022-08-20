open Vect
open Typedefs

(* Camera: pos, screen_top_left, step_right, step_down *)
type cam = vect * vect * vect * vect

let new_cam (pos: vect) (dir: vect) (up: vect) (va: float) : cam =
  if (dot dir up) <> 0.0 then failwith "Tried to create a camera with non-
  perpendicular facing and up directions" else
  let dir = normalise dir in
  let up = normalise up in
  let height = float Params.height in
  let width = float Params.width in
  let screen_width = 2.0 *. (Float.tan (va /. 2.0)) in
  let screen_height = screen_width *. (height /. width) in
  let left = normalise (cross dir up) in
  (pos,
  (pos + dir) + (scalar_mul up (screen_height /. 2.0)) + (scalar_mul left (screen_width /. 2.0)),
  scalar_mul (scalar_mul left (-1.0)) (screen_width /. width),
  scalar_mul (scalar_mul up (-1.0)) (screen_height /. height))

let ray (camera: cam) row col : ray =
  let pos, screen_top_left, step_right, step_down = camera in
  {pos = pos;
   dir = normalise (screen_top_left + (step_down * (float row)) +
            (step_right * (float col)) - pos);
  }
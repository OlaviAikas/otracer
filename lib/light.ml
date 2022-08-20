open Vect
open Typedefs

let point_light (pos: vect) (intensity: float) : light =
  let get_contrbution intersect geo_list =
    let d_vec = pos - intersect.pos in
    let d_squared = norm_sq d_vec in
    let d = Float.sqrt d_squared in
    let d_vec = d_vec / d in
    let shifted_pos = intersect.pos + (intersect.normal * 0.000001) in
    let ray_to_light = {pos = shifted_pos; dir = d_vec;} in
    let closest_int = Scene.closest_intersection ray_to_light geo_list in
    if (closest_int.normal <> zero) && (dist closest_int.pos shifted_pos) < d
    then 0.0 else let angle_contribution = dot intersect.normal d_vec in
    (intensity *. angle_contribution) /. (4.0 *. Float.pi *. Float.pi *. d_squared)
  in
  get_contrbution
open Vect
open Typedefs

let add_geo (g: geometry) (s: scene) : scene =
  let (gl, ll, pt) = s in
  (g::gl, ll, pt)

let add_light (l: light) (s: scene) : scene =
  let (gl, ll, pt) = s in
  (gl, l::ll, pt)

let closest_intersection (r: ray) (geos: geometry list) : intersection =
  let rec aux (curr_int: intersection) (min_dist: float) 
          (r: ray) (l: geometry list) =
    match l with
      []   -> curr_int
    | h::t -> let i = h r in
              if i.normal = zero then aux curr_int min_dist r t else
              let curr_dist = dist r.pos i.pos in
              if curr_dist <= min_dist then
                aux i curr_dist r t
              else
                aux curr_int min_dist r t
  in
  aux {pos = zero; normal = zero; material = Lambertian; albedo = zero} Float.infinity r geos

let get_contrbution light intersect geo_list =
  match light with
      Pointlight(pos, intensity) ->
        let d_vec = pos - intersect.pos in
        let d_squared = norm_sq d_vec in
        let d = Float.sqrt d_squared in
        let d_vec = d_vec / d in
        let shifted_pos = intersect.pos + (intersect.normal * 0.000001) in
        let ray_to_light = {pos = shifted_pos; dir = d_vec;} in
        let closest_int = closest_intersection ray_to_light geo_list in
        if (closest_int.normal <> zero) && (dist closest_int.pos shifted_pos) < d
        then 0.0 else let angle_contribution = dot intersect.normal d_vec in
        (intensity *. angle_contribution) /. (4.0 *. Float.pi *. Float.pi *. d_squared)

let get_colour (r: ray) (s: scene) (depth: int) : vect =
  let geos, lights, _ = s in
  if depth <= 0 then zero else
  let closest_int = closest_intersection r geos in
  if closest_int.normal = zero then
  zero else match closest_int.material with
    Lambertian -> 
      let accum_light (s: float) (l: light) : float = s +. (get_contrbution l closest_int geos) in
      let tot_light = List.fold_left accum_light 0.0 lights in
      closest_int.albedo * tot_light
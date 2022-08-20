open Vect
open Typedefs

let add_geo (g: geometry) (s: scene) : scene =
  let (gl, ll) = s in
  (g::gl, ll)

let add_light (l: light) (s: scene) : scene =
  let (gl, ll) = s in
  (gl, l::ll)

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

let get_colour (r: ray) (s: scene) (depth: int) : vect =
  let geos, lights = s in
  if depth <= 0 then zero else
  let closest_int = closest_intersection r geos in
  if closest_int.normal = zero then
  (1.0, 1.0, 1.0) else match closest_int.material with
    Lambertian -> 
      let accum_light (s: float) (l: light) : float = s +. (l closest_int geos) in
      let tot_light = List.fold_left accum_light 0.0 lights in
      closest_int.albedo * tot_light
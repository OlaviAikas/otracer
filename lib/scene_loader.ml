open Typedefs
open Vect

let vect_of_list (l: float list) : vect =
  match l with
      [v1; v2; v3] -> (v1, v2, v3)
    | _ -> failwith "Invalid list to vect conversion"

let mat_of_string (s: string) : material =
  match s with
    "Lambertian" -> Lambertian
  | _ -> failwith "Invalid material string"

let unpack_sphere (sphere: Otoml.t) : geometry =
  let rec get_sphere_info (pos: vect) (rad: float) (mat: material) (col: vect) (sphere_list: (string * Otoml.t) list) =
    match sphere_list with
      [] -> (pos, rad, mat, col)
    | ("position", arr)::tail -> get_sphere_info (vect_of_list (Otoml.get_array Otoml.get_float arr)) rad mat col tail
    | ("radius", ofloat)::tail -> get_sphere_info pos (Otoml.get_float ofloat) mat col tail
    | ("material", ostring)::tail -> get_sphere_info pos rad (mat_of_string (Otoml.get_string ostring)) col tail
    | ("colour", arr)::tail -> get_sphere_info pos rad mat (vect_of_list (Otoml.get_array Otoml.get_float arr)) tail
    | _ -> failwith "Invalid field in sphere definition!"
  in
  let sphere_list = Otoml.get_table sphere in
  let (pos, rad, mat, col) = get_sphere_info Vect.zero 0.0 Lambertian Vect.zero sphere_list in
  Sphere.sphere pos rad mat col

let unpack_plane (plane: Otoml.t) : geometry =
  let rec get_plane_info point normal col mat plane_list =
    match plane_list with
      [] -> (point, normal, col, mat)
    | ("point", arr)::tail -> get_plane_info (vect_of_list (Otoml.get_array Otoml.get_float arr)) normal col mat tail
    | ("normal", arr)::tail -> get_plane_info point (vect_of_list (Otoml.get_array Otoml.get_float arr)) col mat tail
    | ("material", ostring)::tail -> get_plane_info point normal col (mat_of_string (Otoml.get_string ostring)) tail
    | ("colour", arr)::tail -> get_plane_info point normal (vect_of_list (Otoml.get_array Otoml.get_float arr)) mat tail
    | _ -> failwith "Invalid field in plane definition!"
  in
  let plane_list = Otoml.get_table plane in
  let (point, normal, col, mat) = get_plane_info Vect.zero Vect.zero Vect.zero Lambertian plane_list in
  Plane.plane point normal mat col

let unpack_pointlight (pl: Otoml.t) : light =
  let rec get_light_info pos intensity light_list =
    match light_list with
      [] -> (pos, intensity)
    | ("position", arr)::tail -> get_light_info (vect_of_list (Otoml.get_array Otoml.get_float arr)) intensity tail
    | ("intensity", ofloat)::tail -> get_light_info pos (Otoml.get_float ofloat) tail
    | _ -> failwith "Invalid field in point light definition!"
  in
  let pl_list = Otoml.get_table pl in
  let (pos, intensity) = get_light_info Vect.zero 0.0 pl_list in
  Light.point_light pos intensity

let load_scene (filename: string) : scene =
  let rec unpack_scene otoml_scene geos lights : scene =
    match otoml_scene with
      [] -> (geos, lights)
    | ("point_light", arr) :: tail -> unpack_scene tail geos (List.rev_append (Otoml.get_array unpack_pointlight arr) lights)
    | ("sphere", arr) :: tail -> unpack_scene tail (List.rev_append (Otoml.get_array unpack_sphere arr) geos) lights
    | ("plane", arr) :: tail -> unpack_scene tail (List.rev_append (Otoml.get_array unpack_plane arr) geos) lights
    | _ -> failwith "Unknown array type in scene file."
  in
  try
    let _ = open_in filename in
    let otoml_scene = Otoml.get_table (Otoml.Parser.from_file filename) in
    unpack_scene otoml_scene [] []
  with e -> failwith ("Error opening file " ^ (Printexc.to_string e))
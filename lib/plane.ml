open Vect
open Typedefs

let plane (point: vect) (normal: vect) (material: material) (albedo: vect) =
  let intersect ray =
    let normal = normalise normal in
    let ray_dir_dot_plane_normal = dot ray.dir normal in
    if ray_dir_dot_plane_normal = 0.0 then empty_intersection else
    let t = -.(dot (ray.pos - point) normal) /. ray_dir_dot_plane_normal in
    if t < 0.0 then empty_intersection else
    if ray_dir_dot_plane_normal < 0.0 then
      {pos = ray.pos + (ray.dir * t);
       normal = normal;
       material = material;
       albedo = albedo;}
    else
      {pos = ray.pos + (ray.dir * t);
       normal = normal * -1.0;
       material = material;
       albedo = albedo;} in
  intersect
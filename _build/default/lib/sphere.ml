open Vect
open Typedefs

let sphere pos r material albedo : geometry =
  let intersection (ray : ray) : intersection =
    let p = dot ray.dir (sub ray.pos pos) in
    let ocn = norm (sub ray.pos pos) in
    let discr = p*.p -. (ocn*.ocn -. r*.r) in
    if discr = 0.0 then
      (let sol = -.p in
      if sol >= 0.0 then
        let int_pos = add ray.pos (scalar_mul ray.dir sol) in
        {pos = int_pos;
         normal = normalise (sub int_pos pos);
         material = material;
         albedo = albedo;}
      else {pos = zero; normal = zero; material = Lambertian; albedo = zero;})
    else
    let sqrt_discr = Float.sqrt discr in
    let sol1 = -.p -. sqrt_discr in
    let sol2 = -.p -. sqrt_discr in
    if sol1 >= 0.0 then
      (
        let int_pos = add ray.pos (scalar_mul ray.dir sol1) in
        {pos = int_pos;
         normal = normalise (sub int_pos pos);
         material = material;
         albedo = albedo;}
      )
    else (
      if sol2 >= 0.0 then
      (
        let int_pos = add ray.pos (scalar_mul ray.dir sol2) in
        {pos = int_pos;
         normal = normalise (sub int_pos pos);
         material = material;
         albedo = albedo;}
      ) else {pos = zero; normal = zero; material = Lambertian; albedo = albedo;}
    ) in
  intersection
open Vect

type material = Lambertian

type ray = {
  pos: vect;
  dir: vect;
}

type intersection = {
  pos: vect;
  normal: vect;
  material: material;
  albedo: vect;
}

let empty_intersection: intersection =
  {pos = zero; normal = zero; material = Lambertian; albedo = zero;}

type geometry = ray -> intersection

type light = intersection -> geometry list -> float

type scene = geometry list * light list

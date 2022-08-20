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

type geometry = ray -> intersection

type light = intersection -> geometry list -> float

type scene = geometry list * light list

open Vect

type material = Lambertian

type ray = {
  pos: vect;
  dir: vect;
}

type photon = {
  pos: vect;
  colour: vect;
}

type light = Pointlight of vect * float

type node =
    None
  | Some of int * photon * node * node

type photon_tree = node

type intersection = {
  pos: vect;
  normal: vect;
  material: material;
  albedo: vect;
}

let empty_intersection: intersection =
  {pos = zero; normal = zero; material = Lambertian; albedo = zero;}

type geometry = ray -> intersection

type scene = geometry list * light list * photon_tree


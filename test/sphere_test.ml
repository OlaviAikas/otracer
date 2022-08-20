open Raytracing.Sphere
open Raytracing.Vect

let tvect = Alcotest.testable pretty_print eq

let test_intersection1 () =
  let s = sphere (10.0, 0.0, 0.0) 1.0 Lambertian zero in
  let r: Raytracing.Typedefs.ray = {pos = zero; dir = (1.0, 0.0, 0.0)} in
  Alcotest.(check tvect) "Intersection 1" (9.0, 0.0, 0.0) ((s r).pos)

let test_intersection2 () =
  let s = sphere (10.0, 0.0, 0.0) 1.0 Lambertian zero in
  let r: Raytracing.Typedefs.ray = {pos = (10.0, 5.0, 0.0); dir = (0.0, -1.0, 0.0)} in
  Alcotest.(check tvect) "Intersection 2" (10.0, 1.0, 0.0) ((s r).pos)

let () =
  let open Alcotest in
  run "Sphere" [
      "Basic intersection", [
        test_case "X-axis intersect" `Quick test_intersection1;
        test_case "Up-above intersect" `Quick test_intersection2;
      ]
    ]
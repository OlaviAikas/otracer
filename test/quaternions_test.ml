open Raytracing.Quaternion

let tfloat = Alcotest.testable (fun c f -> Fmt.pf c "%f" f) (fun (f1: float) (f2: float) -> (abs_float (f1 -. f2)) < 0.0000001)
let tvect = Alcotest.testable Raytracing.Vect.pretty_print Raytracing.Vect.eq
let tquat = Alcotest.testable pretty_print eq


let test_projections () =
  let q = (2.0, 1.0, 0.0, 5.0) in
  let () = Alcotest.(check tfloat) "Real proj" 2.0 (rp q) in
  Alcotest.(check tvect) "Vect proj" (1.0, 0.0, 5.0) (vp q)

let test_operations () =
  let () = Alcotest.(check tquat) "Conjugate" (1.0, -.2.0, -.3.0, -.4.0) (conj (1.0, 2.0, 3.0, 4.0)) in
  let () = Alcotest.(check tquat) "Addition" (1.0, 2.0, 3.0, 4.0) (add (1.0, 1.0, 1.0, 1.0) (0.0, 1.0, 2.0, 3.0)) in
  let () = Alcotest.(check tquat) "Subtraction" (1.0, 0.0, 0.0, 0.0) (sub (2.0, 1.0, 2.0, 3.0) (1.0, 1.0, 2.0, 3.0)) in
  Alcotest.(check tquat) "Multiplication" (2.0, -.34.0, 8.0, 6.0) (mul (2.0, 3.0, 4.0, 1.0) (-.2.0, -.3.0, 2.0, -.5.0))

let test_rotation () =
  let rot = rotation (0.0, 0.0, 1.0) (Float.pi /. 2.0) in
  Alcotest.(check tvect) "Rotated vect" (0.0, 1.0, 0.0) (rotate (1.0, 0.0, 0.0) rot)

let () =
  let open Alcotest in
  run "Quat" [
      "real and vect", [
        test_case "Projections" `Quick test_projections
      ];
      "operations", [
        test_case "All ops" `Quick test_operations
      ];
      "rotations", [
        test_case "Rot 1" `Quick test_rotation
      ]
    ]
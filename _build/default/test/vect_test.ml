open Raytracing.Vect

let tvect = Alcotest.testable pretty_print eq
let tfloat = Alcotest.testable (fun c f -> Fmt.pf c "%f" f) (fun (f1: float) (f2: float) -> f1 = f2)

let test_addition_1 () =
  Alcotest.(check tvect) "Basic addition" (3.0, 3.0, 3.0) 
  (add (1.0, 1.0, 1.0) (2.0, 2.0, 2.0))

let test_addition_2 () = 
  Alcotest.(check tvect) "Addition to zero" (0.0, 0.0, 0.0) 
  (add (5.0, 5.0, 5.0) (-5.0, -5.0, -5.0))

let test_addition_3 () = 
  Alcotest.(check tvect) "Adding zero" (1.0, 2.0, 3.0) 
  (add (1.0, 2.0, 3.0) (0.0, 0.0, 0.0))

let test_subtraction_1 () =
  Alcotest.(check tvect) "Basic subtraction" (1.0, 2.0, 3.0) 
  (sub (5.0, 5.0, 5.0) (4.0, 3.0, 2.0))

let test_subtraction_2 () =
  Alcotest.(check tvect) "Subbing negatives" (0.0, 0.0, 0.0) 
  (sub (-5.0, -5.0, -5.0) (-5.0, -5.0, -5.0))

let test_scalar_multiplication () =
  let () = Alcotest.(check tvect) "Basic scalar_mul test 1" (10.0, 5.0, 0.0) (scalar_mul (2.0, 1.0, 0.0) 5.0) in
  Alcotest.(check tvect) "Basic scalar_mul test 2" (0.0, 0.0, 0.0) (scalar_mul (-5.0, -5.0, -5.0) 0.0)

let test_scalar_division () =
  Alcotest.(check tvect) "Basic scalar division test" (2.0, 2.0, 2.0) (scalar_div (4.0, 4.0, 4.0) 2.0)

let test_dot_product () =
  let () = Alcotest.(check tfloat) "Dot product test 1" 0.0 (dot (1.0, 1.0, 0.0) (0.0, 0.0, 1.0)) in
  let () = Alcotest.(check tfloat) "Dot product test 2" 4.0 (dot (2.0, 0.0, 0.0) (2.0, 0.0, 0.0)) in
  Alcotest.(check tfloat) "Dot product test 3" 0.0 (dot (1.0, 0.0, 0.0) (0.0, 1.0, 0.0))

let test_cross_product () =
  let () = Alcotest.(check tvect) "Cross product test 1" (0.0, 0.0, 0.0) (cross(2.0, 0.0, 0.0) (2.0, 0.0, 0.0)) in
  Alcotest.(check tvect) "Cross product test 2" (0.0, 0.0, 1.0) (cross(1.0, 0.0, 0.0) (0.0, 1.0, 0.0))

let test_norm () =
  let () = Alcotest.(check tfloat) "Unit norm test" 1.0 (norm(1.0, 0.0, 0.0)) in
  Alcotest.(check tvect) "Normalisation test" (1.0, 0.0, 0.0) (normalise(3.0, 0.0, 0.0))

let test_operators () =
  let () = Alcotest.(check tvect) "Overloaded +" (10.0, 5.0, 0.0) ((2.0, 1.0, 0.0) + (8.0, 4.0, 0.0)) in
  Alcotest.(check tvect) "Overloaded *" (0.0, 0.0, 0.0) ((-5.0, -5.0, -5.0) * 0.0)


(* Run it *)
let () =
  let open Alcotest in
  run "Vect" [
      "addition", [
        test_case "Basic addition" `Quick test_addition_1;
        test_case "Adding up to 0" `Quick test_addition_2;
        test_case "Adding zero" `Quick test_addition_3;
      ];
      "subtraction", [
        test_case "Basic subtraction" `Quick test_subtraction_1;
        test_case "Negative subtraction" `Quick test_subtraction_2;
      ];
      "scalar ops", [
        test_case "Scalar multiplication" `Quick test_scalar_multiplication;
        test_case "Scalar division" `Quick test_scalar_division;
      ];
      "products", [
        test_case "Dot product" `Quick test_dot_product;
        test_case "Cross product" `Quick test_cross_product;
      ];
      "norm", [
        test_case "Norm tests" `Quick test_norm;
      ];
      "ops", [
        test_case "Overloaded operators" `Quick test_operators;
      ]
    ]
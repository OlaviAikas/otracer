open Raytracing

let ttree = Alcotest.testable Photontree.pretty_print Photontree.eq

let test_insert () =
  let photon1 : Typedefs.photon  = {pos = (1.0, 0.0, 0.0); colour = (0.0, 0.0, 0.0)} in
  let photon2 : Typedefs.photon  = {pos = (0.0, 1.0, 0.0); colour = (0.0, 0.0, 0.0)} in
  let photon3 : Typedefs.photon  = {pos = (0.0, 0.0, 1.0); colour = (0.0, 0.0, 0.0)} in
  let tree = Photontree.insert None photon1 in
  let tree = Photontree.insert tree photon2 in
  let tree = Photontree.insert tree photon3 in
  Alcotest.(check ttree) "Insertion" (Some(0, photon1, Some(1, photon2, Some(2, photon3, None, None), None), None))
  tree

let test_insert_zeroes () =
  let photon1 : Typedefs.photon  = {pos = (0.0, 0.0, 0.0); colour = (0.0, 0.0, 0.0)} in
  let photon2 : Typedefs.photon  = {pos = (0.0, 0.0, 0.0); colour = (0.0, 0.0, 0.0)} in
  let photon3 : Typedefs.photon  = {pos = (0.0, 0.0, 0.0); colour = (0.0, 0.0, 0.0)} in
  let tree = Photontree.insert None photon1 in
  let tree = Photontree.insert tree photon2 in
  let tree = Photontree.insert tree photon3 in
  Alcotest.(check ttree) "Insertion" (Some(0, photon1, None, Some(1, photon2, None, Some(2, photon3, None, None))))
  tree

let () =
  let open Alcotest in
  run "Photontree" [
    "Insertion", [
      test_case "Basic insertion" `Quick test_insert;
      test_case "Zero insertion" `Quick test_insert_zeroes;
  ];
  ]
 
  
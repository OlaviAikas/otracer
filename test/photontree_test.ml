open Raytracing

let ttree = Alcotest.testable Photontree.pretty_print Photontree.eq

let sort_pl (pl: Typedefs.photon list) =
  let order (p1: Typedefs.photon) (p2: Typedefs.photon) =
                    if Vect.eq p1.pos p2.pos then 0 
                    else if p1.pos > p2.pos then 1
                    else -1
  in
  List.sort order pl

let rec pl_eq (pl1: Typedefs.photon list) (pl2: Typedefs.photon list) =
  let pl1 = sort_pl pl1 in
  let pl2 = sort_pl pl2 in
  match pl1, pl2 with
    [], [] -> true
  | [], _ -> false
  | _, [] -> false
  | p1::t1, p2::t2 -> p1.pos = p2.pos && p1.colour = p2.colour && pl_eq t1 t2

let rec pp_pl c (pl: Typedefs.photon list) =
  match pl with
    [] -> Fmt.pf c "[]"
  | h::t -> let () = Vect.pretty_print c h.pos in
            pp_pl c t

let tpl = Alcotest.testable pp_pl pl_eq

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
  Alcotest.(check ttree) "Zero insertion" (Some(0, photon1, None, Some(1, photon2, None, Some(2, photon3, None, None))))
  tree

let test_collect_zeroes () =
  let photon1 : Typedefs.photon  = {pos = (0.0, 0.0, 0.0); colour = (0.0, 0.0, 0.0)} in
  let photon2 : Typedefs.photon  = {pos = (0.0, 0.0, 0.0); colour = (0.0, 0.0, 0.0)} in
  let photon3 : Typedefs.photon  = {pos = (0.0, 0.0, 0.0); colour = (0.0, 0.0, 0.0)} in
  let tree = Photontree.insert None photon1 in
  let tree = Photontree.insert tree photon2 in
  let tree = Photontree.insert tree photon3 in
  Alcotest.(check tpl) "Zero collection" [photon1; photon2; photon3]
  (Photontree.collect tree (0.0, 0.0, 0.0) 5.0)
  
let test_collect_some () =
  let photon1 : Typedefs.photon  = {pos = (0.0, 0.0, 0.0); colour = (0.0, 0.0, 0.0)} in
  let photon2 : Typedefs.photon  = {pos = (1.0, 0.0, 0.0); colour = (0.0, 0.0, 0.0)} in
  let photon3 : Typedefs.photon  = {pos = (0.0, 3.0, 0.0); colour = (0.0, 0.0, 0.0)} in
  let tree = Photontree.insert None photon1 in
  let tree = Photontree.insert tree photon2 in
  let tree = Photontree.insert tree photon3 in
  Alcotest.(check tpl) "Zero collection" [photon1; photon2]
  (Photontree.collect tree (0.0, 0.0, 0.0) 2.0)


let () =
  let open Alcotest in
  run "Photontree" [
    "Insertion", [
      test_case "Basic insertion" `Quick test_insert;
      test_case "Zero insertion" `Quick test_insert_zeroes;
    ];
    "Collection", [
      test_case "Collect zeroes" `Quick test_collect_zeroes;
      test_case "Collect some" `Quick test_collect_some;
    ]
  ]
 
  
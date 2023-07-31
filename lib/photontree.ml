open Typedefs

let dim i v : float =
  let x, y, z = v in
  match i with
      0 -> x
    | 1 -> y
    | 2 -> z
    | _ -> failwith "These vectors only have 3 dimensions!"

let make_node (depth: int) photon : node = Some(depth mod 3, photon, None, None)

let new_tree photon = make_node 0 photon

let rec insert (tree: node) (photon: photon) : node =
    match tree with
        None -> new_tree photon
      | Some(d, p, left, right) ->
        let p_d = dim d p.pos in
        let photon_d = dim d photon.pos in
        if photon_d < p_d then
          match left with
              None -> Some(d, p, (make_node (d + 1) photon), right)
            | _ -> Some(d, p, insert left photon, right)
        else
          match right with
              None -> Some(d, p, left, make_node (d + 1) photon)
            | _ -> Some(d, p, left, insert right photon)

let collect (tree: node) (place: Vect.vect) (radius: float) : photon list =
  let rec search tree place radius res =
    match tree with
        None -> res
      | Some(depth, photon, left, right) ->
        if (Vect.sub photon.pos place |> Vect.norm) < radius then
          List.rev_append (List.rev_append (photon::(search left place radius [])) (search right place radius [])) res
        else
        let place_d = dim depth place in
        let photon_d = dim depth photon.pos in
        let delta = photon_d -. place_d in
        if delta > radius then
          List.rev_append (search right place radius []) res
        else
          List.rev_append (search left place radius []) res
  in
  search tree place radius []
  
let rec eq (tree1: node) (tree2: node) =
  match tree1, tree2 with
    None, None -> true
  | None, _ -> false
  | _, None -> false
  | Some(d1, p1, l1, r1), Some(d2, p2, l2, r2) ->
      d1 == d2 && p1.pos == p2.pos && eq l1 l2 && eq r1 r2

let pretty_print c v =
  let rec print_queue c vl =
    match vl with
      [] -> ()
    | h::t ->
        match h with
          None -> let () = Fmt.pf c "[]" in
                  print_queue c t
        | Some(_, p, l, r) ->
            let () = Vect.pretty_print c p.pos in
            print_queue c (t @ [l] @ [r])
  in
  print_queue c [v]
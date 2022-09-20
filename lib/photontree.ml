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
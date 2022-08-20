open Printf

type vect = float * float * float

let zero = (0.0, 0.0, 0.0)

let pretty_print c v = 
  let v1, v2, v3 = v in
  Fmt.pf c "(%f, %f, %f)" v1 v2 v3

let print_vect (v: vect) =
  let v1, v2, v3 = v in
  printf "(%f, %f, %f)\n" v1 v2 v3

let add (v1: vect) (v2: vect) : vect =
  let v11, v12, v13 = v1 in
  let v21, v22, v23 = v2 in
  (v11 +. v21, v12 +. v22, v13 +. v23)

let sub (v1: vect) (v2: vect) : vect =
  let v11, v12, v13 = v1 in
  let v21, v22, v23 = v2 in
  (v11 -. v21, v12 -. v22, v13 -. v23)

let scalar_mul (v: vect) (s: float) : vect =
  let v1, v2, v3 = v in
  (v1 *. s, v2 *. s, v3 *. s)

let scalar_div (v: vect) (s: float) : vect =
  if s = 0.0 then failwith "Division by 0!" else
  let v1, v2, v3 = v in
  (v1 /. s, v2 /. s, v3 /. s)

let dot v1 v2 =
  let v11, v12, v13 = v1 in
  let v21, v22, v23 = v2 in
  (v11 *. v21 +. v12 *. v22 +. v13 *. v23)

let cross (v1: vect) (v2: vect) : vect =
  let v11, v12, v13 = v1 in
  let v21, v22, v23 = v2 in
  (v12 *. v23 -. v13 *. v22, v13 *. v21 -. v11 *. v23, v11 *. v22 -. v12 *. v21)

let norm_sq (v: vect) : float =
  dot v v

let norm (v: vect) : float =
  v |> norm_sq |> Float.sqrt

let normalise (v: vect) : vect =
  scalar_div v (norm v)

let eq (v1: vect) (v2: vect) =
  norm (sub v1 v2) < 0.0000001

let (+), (-), ( * ), (/) = add, sub, scalar_mul, scalar_div

let dist (v1: vect) (v2: vect) : float = 
  norm (v2 - v1)
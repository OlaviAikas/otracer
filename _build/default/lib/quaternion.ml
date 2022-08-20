type quat = float * float * float * float

let pretty_print c q = 
  let r, x, y, z = q in
  Fmt.pf c "%f + %fi + %fj + %fk)" r x y z

let eq (q1: quat) (q2: quat) =
  let r1, x1, y1, z1 = q1 in
  let r2, x2, y2, z2 = q2 in
  r1 = r2 && x1 = x2 && y1 = y2 && z1 = z2

let rp q = let r, _, _, _ = q in r

let vp q : Vect.vect = let _, x, y, z = q in (x, y, z)

let of_real r = (r, 0.0, 0.0, 0.0)

let of_vect (v: Vect.vect) =
  let b, c, d = v in
  (0.0, b, c, d)

let conj (q: quat) : quat =
  let a, b, c, d = q in
  (a, -.b, -.c, -.d)

let add q1 q2 =
  let r1, x1, y1, z1 = q1 in
  let r2, x2, y2, z2 = q2 in
  (r1 +. r2, x1 +. x2, y1 +. y2, z1 +. z2)

let sub q1 q2 =
  let r1, x1, y1, z1 = q1 in
  let r2, x2, y2, z2 = q2 in
  (r1 -. r2, x1 -. x2, y1 -. y2, z1 -. z2)

let mul q1 q2 =
  let a1, b1, c1, d1 = q1 in
  let a2, b2, c2, d2 = q2 in
  (a1*.a2 -. b1*.b2 -. c1*.c2 -. d1*.d2,
   a1*.b2 +. b1*.a2 +. c1*.d2 -. d1*.c2,
   a1*.c2 -. b1*.d2 +. c1*.a2 +. d1*.b2,
   a1*.d2 +. b1*.c2 -. c1*.b2 +. d1*.a2)

let scalar_mul s q =
  let a, b, c, d = q in
  (s*.a, s*.b, s*.c, s*.d)

let rotation axis angle =
  let normed_axis = Vect.normalise axis in
  let half_angle = angle /. 2.0 in
  add (of_real (Float.cos half_angle)) (scalar_mul (Float.sin half_angle) (of_vect normed_axis))

let rotate (v: Vect.vect) (r: quat) : Vect.vect =
  let qv = of_vect v in
  vp (mul (mul r qv) (conj r))

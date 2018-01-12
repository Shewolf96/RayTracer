type ray = Base_types.ray



module type OBJECT =
sig

  type t
  type config

  val create : config -> t
  val intersect : ray -> t -> (float * point) option
end

module type ObjectInstance =
sig

  module O : OBJECT
  val this : O.t
end




type sphere_t = {position : point; r : float}

module Sphere : OBJECT with type config = sphere_t =
struct

  type config = sphere_t
  type t = sphere_t

  let delta_t a b c = b*.b -. 4.*.a*.c

  let solutions_t a b c = let delta = delta_t a b c in
    if delta < 0. then []
    else if delta = 0. then [-.b/.2.*.a]
    else let sq = Pervasives.sqrt(delta) in [(-.b -. sq)/.2.*.a; (-.b +. sq)/.2.*.a]

  let min_t_solution = function
    | [] -> None
    | t1::t2::_ -> if t1 < t2 && t1 > 0. then Some (t1, (0.,0.,0.))
      else if t2 > 0. then Some (t2, (0.,0.,0.))
      else None
    | t'::_ -> Some (t', (0.,0.,0.)) (* jak policzyc ten punkt????? *)

  let create t = t

  let intersect vec {position=pos; r} = 
    let (x0, y0, z0) = pos and ((p1, p2, p3), (v1, v2, v3)) = vec in
    let a = v1 *. v1 +. v2 *. v2 +. v3 *. v3
    and b = 2.*.p1*.v1 +. 2.*.p2*.v2 +. 2.*.p3*.v3 -. 2.*.v2*.y0 -. 2.*.v3*.z0 -. 2.*.v1*.x0
    and c = p1*.p1 +. p2*.p2 +. p3*.p3 -. 2.*.p1*.x0 -. 2.*.p2*.y0 -. 2.*.p3*.z0 +. x0*.x0 +. y0*.y0 +. z0*.z0 in
    min_t_solution (solutions_t a b c)


end


let make_obj_instance (type a) (module Obj : OBJECT with type config = a) (obj_c) =
  (module struct
    module O = Obj
    let this = O.create obj_c
  end : ObjectInstance) 


(* ........... *)


type point = float * float * float

type vector = point

type ray = point * vector

type color = int * int * int
type normalized_color = point


module type OBJECT =
sig

    type t
    type config

    val create : config -> t
    val intersect : ray -> t -> (float * point) option
end


module type LIGHT =
sig

    type t
    type position
    type parameters

    val create : position -> parameters -> t
    val exposure : point -> vector
end

module type ObjectInstance =
sig

    module O : OBJECT
    val this : O.t
end

let make_obj_instance (type a) (module Obj : OBJECT with type config = a) (obj_c) =
    (module struct
        module O = Obj
        let this = O.create obj_c
    end : ObjectInstance) 
    
    
    (*
    module type PLACE =
    sig
        type t
        
        module O : OBJECT
        type obj = O.t
        
        module L : LIGHT
        type light = L.t
        
        val closest_intersection : vector -> obj list -> obj *  point (* jak tu dać typ modułu OBJECT zamiast 'object'? *)
    end
    
    *)
    (* _____________________________________________________________________________ *)
    
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
    
    module Sphere2 : OBJECT with type config = sphere_t =
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
    
    let ray1 = ((0.,0.,0.),(1.,1.,1.))

    let l = 
      let s = make_obj_instance (module Sphere) {position = (0.,0.,0.); r = 1.}
      and s2 = make_obj_instance (module Sphere2) {position = (0.,0.,0.); r = 2.} in
      [s;s2]
    
      let tescik = match l with
                    (module OInstance)::t -> OInstance.O.intersect ray1 OInstance.this
                    | _ -> failwith ""

        
        
        module Sun : LIGHT =
struct

    type position = direction
    type parameters
    type t = direction * parameters

    let create pos param = (pos, param)
    let exposure p = (p, p) (* ray_trace/intersect? (point, position) *)
    (* To chyba powinno być na zewnątrz, nie w srodku obiektu??
    Ale w sumie to nie wiem xd - moze zamiast koloru, powinno zwracac wektor w strone swiatla? *)
end

let max_dist = 1000000.;;

(*
module MakePlace : PLACE = (* MakePlace (Obj : OBJECT) (Light : LIGHT): PLACE with module O = Obj and module L = Light = *)
struct

    module O : OBJECT
    type obj = O.t

    module L : LIGHT
    type light = L.t

    type t = (obj list) * (light list)


    let closest_intersection vec list_of_obj = let rec aux obj t_factor point = function
                                                [] -> (obj, point)
                                                | h::tail -> let module OB = (val h : OBJECT) in

                                                        let intersect_pair = OB.intersect vec (OB.position, OB.parameters)  in
                                                        match intersect_pair with
                                                            | Some (t', p') ->
                                                                if t' < t_factor then aux (Some h) t' p' tail
                                                                else aux obj t_factor point tail
                                                            | _ ->  aux obj t_factor point tail
                                            in aux None max_dist (0.,0.,0.) list_of_obj

end
 *)
(* _____________________________________________________________________________ *)

let ray_trace () = 0;;

let distance (x1, y1, z1) (x2, y2, z2) = Pervasives.sqrt((x1 -. x2)*.(x1 -. x2) +. (y1 -. y2)*.(y1 -. y2) +. (z1 -.z2)*.(z1 -.z2))

let pixel i j = ((float i), (float j), 0.) (* potem srodek pixela bedziemy brac, a nie jego dolny lewy rog *)

let sight_ray point = (point, (0.,0.,1.)) (* bo na razie patrzymy po prostu wzdluz z-towej wspolrzednej *)


module ObjectFunctor (Object : OBJECT) =
struct

    let intersect vec = Object.intersect vec

end



module LightFunctor (Light : LIGHT) =
struct

    let exposure p = Light.exposure p

end


let closest_intersection vec (list_of_obj) = let rec aux obj t_factor point = function
                                                [] -> (obj, point)
                                                | h::tail -> let module O = (val h : OBJECT) in

                                                        let intersect_pair = O.intersect vec (O.position, O.parameters) in
                                                        match intersect_pair with
                                                            | Some (t', p') ->
                                                                if t' < t_factor then aux (Some h) t' p' tail
                                                                else aux obj t_factor point tail
                                                            | _ ->  aux obj t_factor point tail
                                            in aux None max_dist (0.,0.,0.) list_of_obj




(* let shoot_exposure_ray point light = let LF = LightFunctor (light) in LF.exposure point *)
(*

let compute_ray vec list_of_obj list_of_lights = let rec compute_ray_aux v =
                                                    let (obj, point) = closest_intersection vec list_of_obj in
                                                    let new_v = shoot_exposure_ray point list_of_lights in new_v
                                                in compute_ray_aux new_v
*)


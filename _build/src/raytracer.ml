(* ........... *)


type point = float * float * float

type vector = point

type ray = point * vector

type color = int * int * int

module V = struct

  let add (x1,y1,z1) (x2,y2,z2) = (x1+.x2, y1+.y2, z1+.z2) 
  let mult (x,y,z) c = (x*.c, y*.c, z*.c)

end




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
    type config

    val create : config -> t
    val exposure : t -> point -> vector
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


module type LightInstance =
sig

  module L : LIGHT
  val this : L.t
end

let make_light_instance (type a) (module Light : LIGHT with type config = a) (light_c) =
  (module struct
    module L = Light
    let this = L.create light_c
  end : LightInstance) 
    
    
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
        else if delta = 0. then [-.b/.(2.*.a)]
        else let sq = sqrt(delta) in [(-.b -. sq)/.(2.*.a); (-.b +. sq)/.(2.*.a)]

        let intersection_point (p, v) t = V.add p (V.mult v t)
        
        let min_t_solution ray = function
        | [] -> None
        | t1::t2::_ -> print_float t1; print_newline (); print_float t2; print_newline (); 
            if t1 < t2 && t1 > 0. then Some (t1, intersection_point ray t1)
          else if t2 > 0. then Some (t2, intersection_point ray t2)
        else None
        | t'::_ -> Some (t', intersection_point ray t')
        
        let create t = t
        
        let intersect ray {position=pos; r} = 
        let (x0, y0, z0) = pos and ((p1, p2, p3), (v1, v2, v3)) = ray in
        let a = v1 *. v1 +. v2 *. v2 +. v3 *. v3
        and b = 2.*.p1*.v1 +. 2.*.p2*.v2 +. 2.*.p3*.v3 -. 2.*.v2*.y0 -. 2.*.v3*.z0 -. 2.*.v1*.x0
        and c = p1*.p1 +. p2*.p2 +. p3*.p3 -. 2.*.p1*.x0 -. 2.*.p2*.y0 -. 2.*.p3*.z0 +. x0*.x0 +. y0*.y0 +. z0*.z0 -. r*.r in
        min_t_solution ray (solutions_t a b c)
            
    end


type sun_config = {vec : vector}   

module Sun : LIGHT with type config = sun_config =
struct

  type config = sun_config
  type t = sun_config

  let create t = t
  let exposure {vec} p = V.mult vec (-1.)
end
    
let ray1 = ((0.,0.,0.),(0.,1.,0.))

let l = 
      let s = make_obj_instance (module Sphere) {position = (600.,300.,0.); r = 150.}
      and s2 = make_obj_instance (module Sphere) {position = (50.,100.,0.); r = 20.} in
      [s;s2]
    
let tescik = match l with
                    (module OInstance)::t -> OInstance.O.intersect ray1 OInstance.this
                    | _ -> failwith ""

let lights = 
    let l1 = make_light_instance (module Sun) {vec = (0.,-1.,0.)} in
    [l1]
    
        


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


let closest_intersection ray list_of_obj = 
    let rec aux obj t_factor point = function
                                                [] -> (obj, point)
                                                |(module OInstance : ObjectInstance) as new_obj::t -> 
                                                    let intersect_pair = OInstance.O.intersect ray OInstance.this in
                                                        match intersect_pair with
                                                            | Some (t', p') ->
                                                                if t' < t_factor then aux (Some new_obj) t' p' t
                                                                else aux obj t_factor point t
                                                            | _ ->  aux obj t_factor point t
                                            in aux None max_dist (-1.,-1.,-1.) list_of_obj
 let tescik2 = closest_intersection ray1 l





let shoot_exposure_ray point (module LInstance : LightInstance) = (point, LInstance.L.exposure LInstance.this point)

let compute_ray ray list_of_obj [light] = 
        let (obj, point) = closest_intersection ray list_of_obj in
        match obj with
            None -> (0,0,0)
            | Some (module Obj : ObjectInstance) -> 
                match closest_intersection (shoot_exposure_ray point light) list_of_obj with
                    (None, _) -> (255, 255, 255)
                    | _ -> (100, 100, 100)

        (* let new_v = shoot_exposure_ray point list_of_lights in new_v
                                                in compute_ray_aux new_v *)

let ray_trace x y = compute_ray (sight_ray @@ pixel x y) l lights 


let render () = 
    let open Graphics in
    open_graph " 800x600";
    clear_graph ();
    (* auto_synchronize false; *)
    for x = 0 to 800-1 do
        for y = 0 to 600-1 do
            let (r,g,b) = ray_trace x y in
            set_color @@ rgb r g b;
            plot x y;
        done;
    done;
    synchronize ();

    (* loop forever *)
    let rec loop () : unit = 
      (* let _ = wait_next_event [Mouse_motion; Button_down; Button_up; Key_pressed] in *)
      loop ()
    in
    loop ()
let () = render ()
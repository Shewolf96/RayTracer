(* ..RayTracer.. *)

(*
#load "graphics.cma";;
ocamlc graphics.cma foo.ml
ocamlopt graphics.cmxa foo.ml
 *)

(*
TO DO:
-> camera, focal
-> obracanie obrazu
-> poprawic rozdzielczosc (nie z lewego rogu piksela :P)
-> poprawic to cos z wektorem normalnym plaszczyzny
-> jak to zrobic, zeby dzialalo szybciej????
-> wczytywanie i zapisywanie sceny (przeinstalowac system przy okazji xd)
*)

(* _____________________________________________________________________________ *)


type point = float * float * float

type vector = point

type ray = point * vector

type color = int * int * int

type colorf = float * float * float


(* ________________________SIGNATURES___________________________________________ *)


module type OBJECT =
sig

    type t
    type config

    val create : config -> t
    val get_color : t -> colorf
    val get_diffusion : t -> float
    val reflection_ray : t -> point -> vector -> vector
    val normal_vec : t -> point -> vector
    val intersect : ray -> t -> (float * point) option
end


module type LIGHT =
sig

    type t
    type config

    val create : config -> t
    val exposure : t -> point -> colorf -> vector -> point * vector * colorf
end


module type ObjectInstance =
sig

    module O : OBJECT
    val this : O.t
end


module type LightInstance =
sig

  module L : LIGHT
  val this : L.t
end

module type RAYTRACE =
sig
    type t
    val ray_trace : t -> int -> int -> point
end

(* _____________________________________________________________________________ *)

module V = struct

  let add (x1,y1,z1) (x2,y2,z2) = (x1+.x2, y1+.y2, z1+.z2)
  let mult (x,y,z) c = (x*.c, y*.c, z*.c)
  let mult2 (x1,y1,z1) (x2,y2,z2) = (x1*.x2, y1*.y2, z1*.z2)
  let minus v = mult v (-1.)
  let length (x,y,z) = sqrt (x*.x +. y*.y +. z*.z)
  let normalize (x,y,z) = let len = length (x,y,z) in (x/.len, y/.len, z/.len)
  let dot (x1,y1,z1) (x2,y2,z2) = x1*.x2 +. y1*.y2 +. z1*.z2
  let cross (x1,y1,z1) (x2,y2,z2) = ((y1*.z2 -. z1*.y2), (z1*.x2 -. x1*.z2), (x1*.y2 -. y1*.x2))
  let sub (x1,y1,z1) (x2,y2,z2) = (x1-.x2, y1-.y2, z1-.z2)

  let float_to_int (x,y,z) = (int_of_float x, int_of_float y, int_of_float z)
  let int_to_float (x,y,z) = (float_of_int x, float_of_int y, float_of_int z)
  let to_colorf v = let (x,y,z) = int_to_float v in (x/.255., y/.255., z/.255.)
  let from_colorf (x,y,z) = float_to_int (x*.255., y*.255., z*.255.)
  let in_bouds_255 (x,y,z) = let x' = if x > 255 then 255 else x in
                                let y' = if y > 255 then 255 else y in
                                let z' = if z > 255 then 255 else z in
                            (x',y',z')

  let distance (x1, y1, z1) (x2, y2, z2) = sqrt((x1 -. x2)*.(x1 -. x2) +. (y1 -. y2)*.(y1 -. y2) +. (z1 -.z2)*.(z1 -.z2))

  let print_vec (x,y,z) = print_char '(';
                          print_float(x); print_char ',';
                          print_float(y); print_char ',';
                          print_float(z); print_char ')';;
end

(* _____________________________________________________________________________ *)

module M = struct

    let abs x = if x < 0. then x*.(-1.) else x

    let delta_t a b c = b*.b -. 4.*.a*.c

    let solutions_t a b c = let delta = delta_t a b c in
    if delta < 0. then []
    else if delta = 0. then [-.b/.(2.*.a)]
    else let sq = sqrt(delta) in [(-.b -. sq)/.(2.*.a); (-.b +. sq)/.(2.*.a)]

    let intersection_point (p, v) t = V.add p (V.mult v t)

    let min_t_solution ray = function
        | [] -> None
        | t1::t2::_ ->
            if t1 < t2 && t1 > 0. then Some (t1, intersection_point ray t1)
            else if t2 > 0. then Some (t2, intersection_point ray t2)
        else None
        | t'::_ -> Some (t', intersection_point ray t')
end

(* _____________________________________________________________________________ *)

let max_dist = 1000000.
let shadow_colorf = (0.02,0.02,0.02)
let light_intensity = 300.

(* _____________________________________________________________________________ *)

type sphere_t = {position : point; r : float; color : colorf; diffusion : float}

module Sphere : OBJECT with type config = sphere_t =
    struct

        type config = sphere_t
        type t = sphere_t


        let create t = t

        let get_color {color} = color
        let get_diffusion {diffusion} = diffusion

        let normal_vec {position = pos} point = V.normalize @@ V.add point (V.minus pos)

        let reflection_ray ({position; diffusion} as t) p v =
            let (n1,n2,n3) = normal_vec t p
            and v' = V.normalize v in
            let (a1,a2,a3) = V.mult v' (-1.) in
                (-.(a1*.n3*.n3 -. 2.*.a3*.n1*.n3 +. a1*.n2*.n2 -. 2.*.a2*.n1*.n2 -. a1*.n1*.n1),
                -.(a2*.n3*.n3 -. 2.*.a3*.n2*.n3 -. a2*.n2*.n2 -. 2.*.a1*.n1*.n2 +. a2*.n1*.n1),
                (a3*.n3*.n3 +. (2.*.a2*.n2 +. 2.*.a1*.n1)*.n3 -. a3*.n2*.n2 -. a3*.n1*.n1))


        let intersect ray {position=pos; r} =
        let (x0, y0, z0) = pos and ((p1, p2, p3), (v1, v2, v3)) = ray in
        let a = v1 *. v1 +. v2 *. v2 +. v3 *. v3
        and b = 2.*.p1*.v1 +. 2.*.p2*.v2 +. 2.*.p3*.v3 -. 2.*.v2*.y0 -. 2.*.v3*.z0 -. 2.*.v1*.x0
        and c = p1*.p1 +. p2*.p2 +. p3*.p3 -. 2.*.p1*.x0 -. 2.*.p2*.y0 -. 2.*.p3*.z0 +. x0*.x0 +. y0*.y0 +. z0*.z0 -. r*.r in
        M.min_t_solution ray (M.solutions_t a b c)
end

(* _______________________________________________________________________________*)

type plane_t = {a : float; b : float; c : float; d : float; color : colorf; diffusion : float}

module Plane : OBJECT with type config = plane_t =
    struct

        type config = plane_t
        type t = plane_t


        let create {a; b; c; d; color; diffusion} =
        	let l = V.length (a,b,c) in
        	{a = a/.l; b = b/.l
        	; c = c/.l; d = d/.l
        	; color; diffusion}

        let get_color {color} = color
        let get_diffusion {diffusion} = diffusion

        let normal_vec {a; b; c} _ = (a,b,c)


        let reflection_ray t p v =
            let (n1,n2,n3) = normal_vec t p
            and v' = V.normalize v in
            let (a1,a2,a3) = V.mult v' (-1.) in
                (-.(a1*.n3*.n3 -. 2.*.a3*.n1*.n3 +. a1*.n2*.n2 -. 2.*.a2*.n1*.n2 -. a1*.n1*.n1),
                -.(a2*.n3*.n3 -. 2.*.a3*.n2*.n3 -. a2*.n2*.n2 -. 2.*.a1*.n1*.n2 +. a2*.n1*.n1),
                (a3*.n3*.n3 +. (2.*.a2*.n2 +. 2.*.a1*.n1)*.n3 -. a3*.n2*.n2 -. a3*.n1*.n1))


        let intersect (p, v) {a;b;c;d} =
            let abc = (a,b,c)
            and v' = V.normalize v in
            let num = V.dot v' abc in
                if num = 0. then None
                else
                    let t = (-.(d +. V.dot abc p) /. num) in
                    if t < 0. then None
                    else Some (t, M.intersection_point (p,v') t)
end

(* _______________________________________________________________________________*)

type sun_config = {vec : vector}

module Sun : LIGHT with type config = sun_config =
struct

  type config = sun_config
  type t = sun_config

  let create t = t
  let exposure {vec} p c normal_v =
            let exp_v = V.normalize @@ V.mult vec (-1.) in
            (p, exp_v, V.mult c (V.dot exp_v normal_v))
end

type lamp_config = {position : point; color : colorf}

module Lamp : LIGHT with type config = lamp_config =
struct

    type config = lamp_config
    type t = lamp_config

    let create t = t
    let exposure {position; color} p c normal_v =
        let dist = (V.length @@ V.sub position p)/.light_intensity in
        let exp_v = V.normalize @@ V.sub position p in
        let new_c = V. mult2 (V.mult2 (V.mult c (M.abs (V.dot exp_v normal_v))) (V.mult c (1./.dist/.dist))) color in
        (p, exp_v, new_c)

end

(* _____________________________________________________________________________ *)

type scene_config = {list_of_obj : (module ObjectInstance) list
              ; list_of_lights : (module LightInstance) list}

module MakeScene =
struct

    type t = scene_config


    let empty () = {list_of_obj = []; list_of_lights = []}

    let make_obj_instance (type a) (module Obj : OBJECT with type config = a) (obj_c) =
        (module struct
            module O = Obj
            let this = O.create obj_c
        end : ObjectInstance)

    let make_light_instance (type a) (module Light : LIGHT with type config = a) (light_c) =
      (module struct
        module L = Light
        let this = L.create light_c
      end : LightInstance)


    let add_obj {list_of_obj; list_of_lights} (type a) (module Obj : OBJECT with type config = a) (obj_c) =
        {list_of_obj = (make_obj_instance (module Obj) obj_c)::list_of_obj; list_of_lights}

    let add_light {list_of_lights; list_of_obj} (type a) (module Light : LIGHT with type config = a) (light_c) =
        {list_of_lights = (make_light_instance (module Light) light_c)::list_of_lights; list_of_obj}


end

(* _______________________________________________________________________________ *)

module RayTrace : RAYTRACE with type t = scene_config =
struct


    type t = scene_config


    let create t = t

    let pixel i j = ((float i), (float j), 0.) (* potem srodek pixela bedziemy brac, a nie jego dolny lewy rog *)

    let sight_ray point = (point, (0.,0.,1.)) (* bo na razie patrzymy po prostu wzdluz z-towej wspolrzednej *)

    let closest_intersection ray list_of_obj =
            let rec aux res_pair dist = function
                    [] -> res_pair
                    |(module OInstance : ObjectInstance) as new_obj::t ->
                        let intersect_pair = OInstance.O.intersect ray OInstance.this in
                        match intersect_pair with
                            | Some (new_dist, p') ->
                                if new_dist < dist && new_dist > 0. then aux (Some (new_obj, p')) new_dist t
                                else aux res_pair dist t
                            | _ ->  aux res_pair dist t
                    in aux None max_dist list_of_obj


    let compute_color ((module OInstance : ObjectInstance), point) list_of_obj list_of_lights =
                let rec aux cur_c = function
                        [] -> cur_c
                        | (module LInstance : LightInstance)::t ->
                                    let norm_v = OInstance.O.normal_vec OInstance.this point in
                                    let obj_col = OInstance.O.get_color OInstance.this in
                                    let (p,v,c) =
                                        LInstance.L.exposure LInstance.this point obj_col norm_v in
                                        match closest_intersection (V.add p (V.mult v 0.001),v) list_of_obj with
                                            |None -> let new_c = V.add cur_c c in aux new_c t
                                            |Some(_,_) -> aux cur_c t
                 in aux shadow_colorf list_of_lights


    let rec compute_ray ray list_of_obj list_of_lights depth =
            match closest_intersection ray list_of_obj with
                None -> (0.,0.,0.)
                | Some op_pair ->
                        let ((module OInstance : ObjectInstance), point) = op_pair in
                        if OInstance.O.get_diffusion OInstance.this < 1. && depth > 0
                            then let (_, ray_vec) = ray in
                                let reflected_ray = (point, OInstance.O.reflection_ray OInstance.this point ray_vec) in
                                V.mult2 (OInstance.O.get_color OInstance.this) (compute_ray reflected_ray list_of_obj list_of_lights (depth - 1))
                        else compute_color op_pair list_of_obj list_of_lights


    let ray_trace {list_of_obj; list_of_lights} =
        fun x y -> compute_ray (sight_ray @@ pixel x y) list_of_obj list_of_lights 4
end
(* _____________________________________________________________________________ *)

let sphere_configs =
      [((module Sphere : OBJECT with type config = sphere_t), {position = (600.,300.,150.); r = 150.; color = V.to_colorf (200, 0, 200); diffusion = 1.});
      ((module Sphere), {position = (50.,100.,0.); r = 20.; color = V.to_colorf (255, 0, 0); diffusion = 1.});
      ((module Sphere), {position = (150.,400.,200.); r = 50.; color = V.to_colorf (255, 200, 0); diffusion = 1.});
      ((module Sphere), {position = (350.,250.,50.); r = 80.; color = V.to_colorf (150, 150, 150); diffusion = 0.});
      ((module Sphere), {position = (400.,500.,600.); r = 300.; color = V.to_colorf (50, 50, 200); diffusion = 1.})]

let plane_configs =
      [((module Plane : OBJECT with type config = plane_t), {a = 0.; b = 0.5; c = -0.2; d = 10.; color = V.to_colorf (20, 50, 50); diffusion = 1.});]

(*
let plane_configs =
      [((module Plane : OBJECT with type config = plane_t), {a = 0.; b = 0.5; c = -0.2; d = 10.; color = V.to_colorf (20, 50, 50); diffusion = 1.});
      ((module Plane : OBJECT with type config = plane_t), {a = -1.; b = 0.3; c = -0.5; d = 800.; color = V.to_colorf (50, 0, 30); diffusion = 1.})]
*)

let sun_configs =[((module Sun : LIGHT with type config = sun_config), {vec = (0.1,-1.,0.1)})]
let lamp_configs = [((module Lamp : LIGHT with type config = lamp_config), {position = (100., 700., 0.); color = V.to_colorf (0, 200, 255)});
                    ((module Lamp), {position = (500., -100., 400.); color = V.to_colorf (200, 20, 255)});
                    ((module Lamp), {position = (700., -50., -400.); color = V.to_colorf (0, 255, 0)})]


let myObjects = List.fold_right (fun (x,y) scene -> MakeScene.add_obj scene x y)
                        plane_configs
                        (List.fold_right (fun (x,y) scene -> MakeScene.add_obj scene x y) sphere_configs (MakeScene.empty ()))

let myScene = List.fold_right (fun (x,y) scene -> MakeScene.add_light scene x y) lamp_configs
                        (List.fold_right (fun (x,y) scene -> MakeScene.add_light scene x y) sun_configs myObjects)

(* _____________________________________________________________________________ *)

(*
type camera_t = {a : point; b : point; c : point
                ; focal : point
                ; img_width : int; img_height : int
                ; w : float; h : float
                ; pix_w : float; pix_h : float}


module type CAMERA =
sig
    type config
    type t

    val create : config -> t

end


module Camera : CAMERA with type config = camera_t =
struct

    type config = camera_t
    type t = camera_t

    let create {a; b; c; focal; img_width; img_height} =
            let w = V.distance a b
            and h = V.distance a c in
            let pix_w = w /. float_of_int img_width
            and pix_h = h /. float_of_int img_height in
        {a; b; c; focal; img_width; img_height; w; h; pix_h; pix_w}

    let pixel_coords {a; b; c; w; h} i j = ()

end *)

(* let test1 = myScene
let test2 = myObjects
let test3 = RayTrace.ray_trace myScene
 *)


let render () =
    let open Graphics in
    open_graph " 800x600";
    clear_graph ();
    (* auto_synchronize false; *)
    let img_width = 800
    and img_height = 600 in
    for x = 0 to (img_width-1) do
        for y = 0 to (img_height-1) do
            let (r,g,b) = V.in_bouds_255 @@ V.from_colorf @@ RayTrace.ray_trace myScene x y in
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



(*
let objects =
      let s = make_obj_instance (module Sphere) {position = (600.,300.,150.); r = 150.; color = V.to_colorf (200, 0, 200); diffusion = 1.}
      and s2 = make_obj_instance (module Sphere) {position = (50.,100.,0.); r = 20.; color = V.to_colorf (255, 0, 0); diffusion = 1.}
      and s3 = make_obj_instance (module Sphere) {position = (150.,400.,200.); r = 50.; color = V.to_colorf (255, 200, 0); diffusion = 1.}
      and s4 = make_obj_instance (module Sphere) {position = (350.,250.,50.); r = 80.; color = V.to_colorf (150, 150, 150); diffusion = 0.}
      and s5 = make_obj_instance (module Sphere) {position = (400.,500.,600.); r = 300.; color = V.to_colorf (50, 50, 200); diffusion = 1.}
      and s6 = make_obj_instance (module Plane) {a = 0.; b = 0.5; c = -1.; d = 400.; color = V.to_colorf (80, 50, 80); diffusion = 1.} in
      [s;s2;s3;s4;s5;]


let lights =
    let l1 = make_light_instance (module Sun) {vec = (0.,-1.,0.1)}
    and l2 = make_light_instance (module Lamp) {position = (100., 700., 0.); color = V.to_colorf (0, 200, 255)}
    and l3 = make_light_instance (module Lamp) {position = (500., -100., 400.); color = V.to_colorf (200, 20, 255)}
    and l4 = make_light_instance (module Lamp) {position = (700., -50., -400.); color = V.to_colorf (0, 255, 0)} in
    [l1;l2;l3;l4]
 *)

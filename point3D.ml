
type t = {x: float; y:float; z:float}

let create ~x ~y ~z = {x;y;z}

let add p0 p1 =
    {x=(p0.x +. p1.x);y=(p0.y +. p1.y);z=(p0.z +. p1.z)}

let sub p0 p1 =
    {x=(p0.x -. p1.x);y=(p0.y -. p1.y);z=(p0.z -. p1.z)}

let mult p0 a =
    {x=(p0.x *. a);y=(p0.y *. a);z=(p0.z *. a)}

let getX p0 = p0.x
let getY p0 = p0.y
let getZ p0 = p0.z

(* computes the intersection between a segment passing by p0 and p1 and a plane z=d *)
let computeLineAndAPlaneIntersection p0 p1 d =
   let zDelta= (p0.z -. p1.z) in
   if (zDelta=0.0) then
   None
   else
      let u=(p0.z -. d) /. zDelta in
      let diff= sub p1 p0 in
      let delta= mult diff u in
      let interpolated = add p0 delta in
      Some interpolated	   
		   
let getPointFromLine ~p0 ~p1 ~d =
    let v0=p0.z in
    let v1=p1.z in
    if (d>v0 && d>v1) then
       []
    else
	if (d<v0 && d<v1) then
	   []
	else
           if (d=v0 && d=v1) then
	      p0::p1::[]
	   else
	      if (d=v0) then
	      	 p0::[]
	      else
		if (d=v1) then
		   p1::[]
		 else
                   let intersection=computeLineAndAPlaneIntersection p0 p1 d in
                   match intersection with
                   | None -> []
                   | Some point -> point::[]

  

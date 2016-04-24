
type t = {points:Point3D.t list; first: Point3D.t; last:Point3D.t}
		      
let create ~points =
  match points with
  | [] -> None
  | [p] as l -> Some {points=l; first=p;last=p}
  | _ as l ->
     let points= ListUtils.remove_consecutive_duplicates l in
     let first = List.hd points in
     let last = ListUtils.last points in
     Some {points; first;last}		       

let to_pointList ~polyline = polyline.points
			       
let merge ~poly0 ~poly1 =
  if (poly0.first=poly1.first) then
    let points=List.append (List.rev poly0.points) poly1.points in
    create points
  else
    if (poly0.last=poly1.last) then
      let points=List.append poly0.points (List.rev poly1.points) in
      create points
    else
      if (poly0.first=poly1.last) then
	let points=List.append poly1.points poly0.points in
        create points
      else
	if (poly0.last=poly1.first) then
	  let points=List.append  poly0.points poly1.points in
	  create points
	else
	  None
 

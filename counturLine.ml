
let rec printPoints  = function
  | [] -> Printf.printf "end of line\n\r"
  | p::tl ->
        Printf.printf ">point %f %f %f\n\r" (Point3D.getX p) (Point3D.getY p) (Point3D.getZ p);
        printPoints tl
		    
let getPoint p0 p1 p2 z: PolyLine.t option =
    let points0= Point3D.getPointFromLine p0 p1 z in
    let points1= Point3D.getPointFromLine p1 p2 z in
    let points2= Point3D.getPointFromLine p2 p0 z in
    let allPoints = (List.append points0 points1) |> List.append points2 in
    printPoints allPoints;
    PolyLine.create allPoints
    
let getIntersection ~l ~r ~t ~b ~vLT ~vRT ~vLB ~vRB ~d =
    let open Point3D in
    let pLT =create l t vLT in
    let pRT =create r t vRT in
    let pLB =create l b vLB in
    let pRB =create r b vRB in
    let poly0 = getPoint pLT pRT pRB d in
    let poly1 = getPoint pLT pRB pLB d in
    match poly0, poly1 with
    | None, None -> []
    | Some p0, None -> p0::[]
    | None, Some p1 -> p1::[]
    | Some p0, Some p1 -> match PolyLine.merge p0 p1 with
			  | None -> p0::p1::[]
			  | Some p -> p::[]

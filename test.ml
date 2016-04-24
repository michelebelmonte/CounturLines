
let rec printPoints  = function
  | [] -> Printf.printf "end of line\n\r"
  | p::tl ->
        Printf.printf "point %f %f %f\n\r" (Point3D.getX p) (Point3D.getY p) (Point3D.getZ p);
        printPoints tl

let rec printLines = function
  | [] -> Printf.printf "end of lines\n\r"
  | l::tl ->
      printPoints l;
      printLines tl

let rec printPolylines =function
  | [] -> Printf.printf "end of polylines\n\r"
  | hd::tl ->
     printPoints (PolyLine.to_pointList hd);
     printPolylines tl
     
let () =
  let lines = CounturLine.getIntersection ~l:0.0 ~r:1.0 ~t:1.0 ~b:0.0 ~vLT:5.0 ~vRT:5.0 ~vLB:3.0 ~vRB:5.0 ~d:4.0 in
  Printf.printf "quattro\n\r";
  printPolylines lines;  

	
open IsoLine
	
let () =
  let segments =computeIsoSegments ~quota:1.0 ~left:0.0 ~top:1.0  ~right:1.0 ~bottom:0.0 ~leftTopValue:2.0 ~rightTopValue:0.0 ~leftBottomValue:0.0 ~rightBottomValue:0.0 in
  List.iter  (fun x -> Printf.printf "v0.x=%f, v0.y=%f, v1.x=%f, v1.y=%f\r\n" x.v0.x x.v0.y x.v1.x x.v1.y) segments
  
let () =
  let segments =computeIsoSegments ~quota:1.0 ~left:0.0 ~top:1.0  ~right:1.0 ~bottom:0.0 ~leftTopValue:2.0 ~rightTopValue:2.0 ~leftBottomValue:0.0 ~rightBottomValue:0.0 in
  List.iter  (fun x -> Printf.printf "v0.x=%f, v0.y=%f, v1.x=%f, v1.y=%f\r\n" x.v0.x x.v0.y x.v1.x x.v1.y) segments

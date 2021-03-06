	
open IsoLine
	
let () =
  let segments =computeIsoSegments ~quota:1.0 ~left:0.0 ~top:1.0  ~right:1.0 ~bottom:0.0 ~leftTopValue:2.0 ~rightTopValue:0.0 ~leftBottomValue:0.0 ~rightBottomValue:0.0 in
  Printf.printf "Expecting one segment.\r\n";
  List.iter  (fun x -> Printf.printf "v0.x=%f, v0.y=%f, v1.x=%f, v1.y=%f\r\n" x.v0.x x.v0.y x.v1.x x.v1.y) segments
  
let () =
  let segments =computeIsoSegments ~quota:1.0 ~left:0.0 ~top:1.0  ~right:1.0 ~bottom:0.0 ~leftTopValue:2.0 ~rightTopValue:2.0 ~leftBottomValue:0.0 ~rightBottomValue:0.0 in
  Printf.printf "Expecting one segment.\r\n";
  List.iter  (fun x -> Printf.printf "v0.x=%f, v0.y=%f, v1.x=%f, v1.y=%f\r\n" x.v0.x x.v0.y x.v1.x x.v1.y) segments
  
let () =
  let segments =computeIsoSegments ~quota:1.0 ~left:0.0 ~top:1.0  ~right:1.0 ~bottom:0.0 ~leftTopValue:2.0 ~rightTopValue:0.0 ~leftBottomValue:0.0 ~rightBottomValue:2.0 in
  Printf.printf "Expecting two segments.\r\n";
  List.iter  (fun x -> Printf.printf "v0.x=%f, v0.y=%f, v1.x=%f, v1.y=%f\r\n" x.v0.x x.v0.y x.v1.x x.v1.y) segments
  
let () =
  let segments =computeIsoSegments ~quota:2.0 ~left:0.0 ~top:1.0  ~right:1.0 ~bottom:0.0 ~leftTopValue:0.0 ~rightTopValue:2.0 ~leftBottomValue:2.0 ~rightBottomValue:0.0 in
  Printf.printf "Expecting two (degenerate) segments.\r\n";
  List.iter  (fun x -> Printf.printf "v0.x=%f, v0.y=%f, v1.x=%f, v1.y=%f\r\n" x.v0.x x.v0.y x.v1.x x.v1.y) segments
  
let () =
  let valueList=0.0::0.0::0.0::
                0.0::2.0::0.0::
			    0.0::0.0::0.0::[] in
  let values=Array.of_list valueList in
  let segments =computeIsoLine ~quota:1.0 ~l:0.0 ~t:2.0  ~dx:1.0 ~dy:1.0 ~nx:3 ~ny:3 ~values in
  Printf.printf "Expecting four segments.\r\n";
  List.iter  (fun x -> Printf.printf "v0.x=%f, v0.y=%f, v1.x=%f, v1.y=%f\r\n" x.v0.x x.v0.y x.v1.x x.v1.y) segments
  
let () =
  let valueList=2.0::0.0::0.0::
                2.0::0.0::0.0::
			    2.0::0.0::0.0::[] in
  let values=Array.of_list valueList in
  let segments =computeIsoLine ~quota:1.0 ~l:0.0 ~t:2.0  ~dx:1.0 ~dy:1.0 ~nx:3 ~ny:3 ~values in
  Printf.printf "Expecting two segments.\r\n";
  List.iter  (fun x -> Printf.printf "v0.x=%f, v0.y=%f, v1.x=%f, v1.y=%f\r\n" x.v0.x x.v0.y x.v1.x x.v1.y) segments

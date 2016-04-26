
type valueType = X | O


type point2d={
    x:float;
    y:float;
  }

type point3d={
    x:float;
    y:float;
    z:float
  }

type segment ={
    v0:point2d;
    v1:point2d
  }

type 'a quad = {
    topLeft:'a;
    topRight:'a;
    bottomLeft:'a;
    bottomRight:'a
  }

let getValidity quota currentValue =
  if (currentValue<quota) then
    X
  else
    O

let getValidityQuad quota valueQuad =
  let getValidity = getValidity quota in
  let topLeft=getValidity valueQuad.topLeft.z in
  let topRight=getValidity valueQuad.topRight.z in
  let bottomLeft =getValidity valueQuad.bottomLeft.z in
  let bottomRight=getValidity valueQuad.bottomRight.z in
  {topLeft;topRight;bottomLeft;bottomRight}

let getIntersection quota p0 p1 =
  let zDistance = p1.z -. p0.z in
  let zDelta = quota -. p0.z in
  let zFactor = zDelta /. zDistance in
  if (p0.y = p1.y) then
    let xDelta=p1.x -. p0.x in
    let newX=p0.x +. (xDelta *. zFactor) in
    {x=newX;y=p0.y}
  else
    let yDelta = p1.y -. p0.y in
    let newY=p0.y +. (yDelta *. zFactor) in
    {x=p0.x;y=newY}

let getSegmentFromThreePoints quota pivot p0 p1  =
  let getIntersection = getIntersection quota in
  let v0 = getIntersection pivot p0 in
  let v1 = getIntersection pivot p1 in
  {v0;v1}

let getSegmentFromFourPoints quota valid0 valid1 invalid0 invalid1  =
  let getIntersection = getIntersection quota in
  let v0 = getIntersection valid0 invalid0 in
  let v1 = getIntersection valid1 invalid1 in
  {v0;v1}
  
let getMarchingCubeFunction validityQuad =
  match validityQuad.topLeft, validityQuad.topRight, validityQuad.bottomLeft, validityQuad.bottomRight with
  (*no valid points*)
  | X,X,
    X,X -> fun quad reference-> []

  | O,O, 
    O,O -> fun quad refernce -> []

  (*one valid point or one invalid point*)
  | X,O,
    O,O
  | O,X,
    X,X -> fun quota quad ->
						 let getSegmentFromThreePoints = getSegmentFromThreePoints quota in
						 let s0= getSegmentFromThreePoints quad.topLeft quad.bottomLeft quad.topRight in
						 s0::[]
						       
  | O,X,
    O,O
  | X,O,
    X,X -> fun quota quad ->
						 let getSegmentFromThreePoints = getSegmentFromThreePoints quota in
						 let s0= getSegmentFromThreePoints quad.topRight quad.topLeft quad.bottomRight in
						 s0::[]

  | O,O,
    X,O						       
  | X,X,
    O,X -> fun quota quad ->
						 let getSegmentFromThreePoints = getSegmentFromThreePoints quota in
						 let s0= getSegmentFromThreePoints quad.bottomLeft quad.topLeft quad.bottomRight in
						 s0::[]

  | O,O,
    O,X						       
  | X,X,
    X,O -> fun quota quad ->
						 let getSegmentFromThreePoints = getSegmentFromThreePoints quota in
						 let s0= getSegmentFromThreePoints quad.bottomRight quad.topRight quad.bottomLeft in
						 s0::[]

  (*two valid adjacent points*)
  | O,O,
    X,X -> fun quota quad ->
						       let getSegmentFromFourPoints = getSegmentFromFourPoints quota in
						       let s0 = getSegmentFromFourPoints quad.topLeft quad.topRight quad.bottomLeft quad.bottomRight in
						       s0::[]
  | X,O,
    X,O -> fun quota quad ->
						       let getSegmentFromFourPoints = getSegmentFromFourPoints quota in
						       let s0 = getSegmentFromFourPoints quad.topRight quad.bottomRight quad.topLeft quad.bottomRight in
						       s0::[]
  | X,X,
    O,O -> fun quota quad ->
						       let getSegmentFromFourPoints = getSegmentFromFourPoints quota in
						       let s0 = getSegmentFromFourPoints quad.bottomLeft quad.bottomRight quad.topLeft quad.topRight in
						       s0::[]
  | O,X,
    O,X -> fun quota quad->
						       let getSegmentFromFourPoints = getSegmentFromFourPoints quota in
						       let s0 = getSegmentFromFourPoints quad.topLeft quad.bottomLeft quad.topRight quad.bottomRight in
						       s0::[]

  (*add a test to check if the segments are the same*)
  (*two opposite valid points*)
  | O,X,
    X,O -> fun quota quad ->
						       let getSegmentFromThreePoints = getSegmentFromThreePoints quota in
						       let s0= getSegmentFromThreePoints quad.topLeft quad.bottomLeft quad.topRight in
						       let s1= getSegmentFromThreePoints quad.bottomRight quad.topRight quad.bottomLeft in
						       s0::s1::[]
							        
  | X,O,
    O,X -> fun quota quad ->
						       let getSegmentFromThreePoints = getSegmentFromThreePoints quota in
						       let s0= getSegmentFromThreePoints quad.topRight quad.topLeft quad.bottomRight in
						       let s1= getSegmentFromThreePoints quad.bottomLeft quad.topLeft quad.bottomRight in
						       s0::s1::[]

let getIsoSegments quota quad =
  let validityQuad = getValidityQuad quota quad in
  let marchingSquareFunction=getMarchingCubeFunction validityQuad in
  marchingSquareFunction quota quad

let computeIsoSegments ~quota ~left ~top  ~right ~bottom ~leftTopValue ~rightTopValue ~leftBottomValue ~rightBottomValue =
  let topLeft={x=left;y=top;z=leftTopValue} in
  let topRight={x=right;y=top;z=rightTopValue} in
  let bottomLeft={x=left;y=bottom;z=leftBottomValue} in
  let bottomRight={x=left;y=bottom;z=rightBottomValue} in
  let quad={topLeft;topRight;bottomLeft;bottomRight} in
  getIsoSegments quota quad
							     
let () =
  let segments =computeIsoSegments ~quota:1.0 ~left:0.0 ~top:1.0  ~right:1.0 ~bottom:0.0 ~leftTopValue:2.0 ~rightTopValue:0.0 ~leftBottomValue:0.0 ~rightBottomValue:0.0 in
  List.iter  (fun x -> Printf.printf "v0.x=%f, v0.y=%f, v1.x=%f, v1.y=%f\r\n" x.v0.x x.v0.y x.v1.x x.v1.y) segments

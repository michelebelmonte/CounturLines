
type valueType = Smaller | GreaterOrEqual


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

let getValidity referenceValue currentValue =
  if (currentValue<referenceValue) then
    Smaller
  else
    GreaterOrEqual

let getQuadValidities valueQuad reference =
  let getValidity = getValidity reference in
  let topLeft=getValidity valueQuad.topLeft.z in
  let topRight=getValidity valueQuad.topRight.z in
  let bottomLeft =getValidity valueQuad.bottomLeft.z in
  let bottomRight=getValidity valueQuad.bottomRight.z in
  {topLeft;topRight;bottomLeft;bottomRight}

let getIntersection z p0 p1 =
  let zDistance = p1.z -. p0.z in
  let zDelta = z -. p0.z in
  let zFactor = zDelta /. zDistance in
  if (p0.y = p1.y) then
    let xDelta=p1.x -. p0.x in
    let newX=p0.x +. (xDelta *. zFactor) in
    {x=newX;y=p0.y}
  else
    let yDelta = p1.y -. p0.y in
    let newY=p0.y +. (yDelta *. zFactor) in
    {x=p0.x;y=newY}

let getSegmentFromThreePoints reference pivot p0 p1  =
  let getIntersection = getIntersection reference in
  let v0 = getIntersection pivot p0 in
  let v1 = getIntersection pivot p1 in
  {v0;v1}

let getSegmentFromFourPoints reference valid0 valid1 invalid0 invalid1  =
  let getIntersection = getIntersection reference in
  let v0 = getIntersection valid0 invalid0 in
  let v1 = getIntersection valid1 invalid1 in
  {v0;v1}
  
let getMarchingCubeFunction validityQuad =
  match validityQuad.topLeft, validityQuad.topRight, validityQuad.bottomLeft, validityQuad.bottomRight with
  (*no valid points*)
  | Smaller,Smaller, Smaller,Smaller -> fun quad reference-> []

  | GreaterOrEqual, GreaterOrEqual, GreaterOrEqual, GreaterOrEqual -> fun quad refernce -> []

  (*one valid point or one invalid point*)
  | Smaller, GreaterOrEqual,
    GreaterOrEqual,GreaterOrEqual
  | GreaterOrEqual, Smaller,
    Smaller, Smaller -> fun quad reference ->
						 let getSegmentFromThreePoints = getSegmentFromThreePoints reference in
						 let s0= getSegmentFromThreePoints quad.topLeft quad.bottomLeft quad.topRight in
						 s0::[]
						       
  | GreaterOrEqual, Smaller,
    GreaterOrEqual,GreaterOrEqual
  | Smaller, GreaterOrEqual,
    Smaller, Smaller -> fun quad reference ->
						 let getSegmentFromThreePoints = getSegmentFromThreePoints reference in
						 let s0= getSegmentFromThreePoints quad.topRight quad.topLeft quad.bottomRight in
						 s0::[]

  | GreaterOrEqual,GreaterOrEqual,
    Smaller, GreaterOrEqual						       
  | Smaller, Smaller,
    GreaterOrEqual,  Smaller -> fun quad reference ->
						 let getSegmentFromThreePoints = getSegmentFromThreePoints reference in
						 let s0= getSegmentFromThreePoints quad.bottomLeft quad.topLeft quad.bottomRight in
						 s0::[]

  | GreaterOrEqual,GreaterOrEqual,
    GreaterOrEqual,Smaller						       
  | Smaller, Smaller,
    Smaller,  GreaterOrEqual -> fun quad reference ->
						 let getSegmentFromThreePoints = getSegmentFromThreePoints reference in
						 let s0= getSegmentFromThreePoints quad.bottomRight quad.topRight quad.bottomLeft in
						 s0::[]

  (*two valid adjacent points*)
  | GreaterOrEqual,GreaterOrEqual,
    Smaller, Smaller -> fun quad reference ->
						       let getSegmentFromFourPoints = getSegmentFromFourPoints reference in
						       let s0 = getSegmentFromFourPoints quad.topLeft quad.topRight quad.bottomLeft quad.bottomRight in
						       s0::[]
  | Smaller, GreaterOrEqual,
    Smaller, GreaterOrEqual -> fun quad reference ->
						       let getSegmentFromFourPoints = getSegmentFromFourPoints reference in
						       let s0 = getSegmentFromFourPoints quad.topRight quad.bottomRight quad.topLeft quad.bottomRight in
						       s0::[]
  | Smaller, Smaller,
    GreaterOrEqual, GreaterOrEqual -> fun quad reference ->
						       let getSegmentFromFourPoints = getSegmentFromFourPoints reference in
						       let s0 = getSegmentFromFourPoints quad.bottomLeft quad.bottomRight quad.topLeft quad.topRight in
						       s0::[]
  | GreaterOrEqual, Smaller,
    GreaterOrEqual, Smaller -> fun quad reference ->
						       let getSegmentFromFourPoints = getSegmentFromFourPoints reference in
						       let s0 = getSegmentFromFourPoints quad.topLeft quad.bottomLeft quad.topRight quad.bottomRight in
						       s0::[]

  (*aggiungere un test per vedere se due segmenti sono uguali*)
  (*two opposite valid points*)
  | GreaterOrEqual,Smaller,
    Smaller, GreaterOrEqual -> fun quad reference ->
						       let getSegmentFromThreePoints = getSegmentFromThreePoints reference in
						       let s0= getSegmentFromThreePoints quad.topLeft quad.bottomLeft quad.topRight in
						       let s1= getSegmentFromThreePoints quad.bottomRight quad.topRight quad.bottomLeft in
						       s0::s1::[]
							        
  | Smaller, GreaterOrEqual,
    GreaterOrEqual,Smaller -> fun quad reference ->
						       let getSegmentFromThreePoints = getSegmentFromThreePoints reference in
						       let s0= getSegmentFromThreePoints quad.topRight quad.topLeft quad.bottomRight in
						       let s1= getSegmentFromThreePoints quad.bottomLeft quad.topLeft quad.bottomRight in
						       s0::s1::[]
							     
let () =
  let point={x=1.0;y=2.0;z=3.0} in
  Printf.printf "hello! %f\r\n" point.x

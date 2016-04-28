
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
  
let getSegmentList s0 s1 =
	if (s0.v0==s1.v0 && s0.v1==s1.v1) then
		s0::[]
	else
		if (s0.v0==s1.v1 && s0.v1==s1.v0) then
			s0::[]
		else
			s0::s1::[]
			
let computeMarchingSquare quota valueQuad validityQuad =
  match validityQuad.topLeft, validityQuad.topRight, validityQuad.bottomLeft, validityQuad.bottomRight with
  (*no valid points*)
  | X,X,
    X,X -> []

  | O,O, 
    O,O -> []

  (*one valid point or one invalid point*)
  | X,O,
    O,O
	
  | O,X,
    X,X -> 
			let getSegmentFromThreePoints = getSegmentFromThreePoints quota in
			let s0= getSegmentFromThreePoints valueQuad.topLeft valueQuad.bottomLeft valueQuad.topRight in
			s0::[]
						       
  | O,X,
    O,O
	
  | X,O,
    X,X -> 
			let getSegmentFromThreePoints = getSegmentFromThreePoints quota in
			let s0= getSegmentFromThreePoints valueQuad.topRight valueQuad.topLeft valueQuad.bottomRight in
			s0::[]

  | O,O,
    X,O	
	
  | X,X,
    O,X -> 
			let getSegmentFromThreePoints = getSegmentFromThreePoints quota in
			let s0= getSegmentFromThreePoints valueQuad.bottomLeft valueQuad.topLeft valueQuad.bottomRight in
			s0::[]

  | O,O,
    O,X	
	
  | X,X,
    X,O -> 
			let getSegmentFromThreePoints = getSegmentFromThreePoints quota in
			let s0= getSegmentFromThreePoints valueQuad.bottomRight valueQuad.topRight valueQuad.bottomLeft in
			s0::[]

  (*two valid adjacent points*)
  | O,O,
    X,X -> 
			let getSegmentFromFourPoints = getSegmentFromFourPoints quota in
			let s0 = getSegmentFromFourPoints valueQuad.topLeft valueQuad.topRight valueQuad.bottomLeft valueQuad.bottomRight in
			s0::[]
  | X,O,
    X,O -> 
			let getSegmentFromFourPoints = getSegmentFromFourPoints quota in
			let s0 = getSegmentFromFourPoints valueQuad.topRight valueQuad.bottomRight valueQuad.topLeft valueQuad.bottomRight in
			s0::[]
  | X,X,
    O,O ->
			let getSegmentFromFourPoints = getSegmentFromFourPoints quota in
			let s0 = getSegmentFromFourPoints valueQuad.bottomLeft valueQuad.bottomRight valueQuad.topLeft valueQuad.topRight in
			s0::[]
  | O,X,
    O,X ->
			let getSegmentFromFourPoints = getSegmentFromFourPoints quota in
			let s0 = getSegmentFromFourPoints valueQuad.topLeft valueQuad.bottomLeft valueQuad.topRight valueQuad.bottomRight in
			s0::[]

  (*add a test to check if the segments are the same*)
  (*two opposite valid points*)
  | O,X,
    X,O ->
			let getSegmentFromThreePoints = getSegmentFromThreePoints quota in
			let s0= getSegmentFromThreePoints valueQuad.topLeft valueQuad.topRight valueQuad.bottomLeft  in
			let s1= getSegmentFromThreePoints valueQuad.bottomRight valueQuad.bottomLeft valueQuad.topRight in
			getSegmentList s0 s1	        
  | X,O,
    O,X ->
			let getSegmentFromThreePoints = getSegmentFromThreePoints quota in
			let s0= getSegmentFromThreePoints valueQuad.topRight valueQuad.topLeft valueQuad.bottomRight in
			let s1= getSegmentFromThreePoints valueQuad.bottomLeft valueQuad.bottomRight valueQuad.topLeft in
			getSegmentList s0 s1

let getIsoSegments quota valueQuad =
  let validityQuad = getValidityQuad quota valueQuad in
  computeMarchingSquare quota valueQuad validityQuad

let computeIsoSegments ~quota ~left ~top  ~right ~bottom ~leftTopValue ~rightTopValue ~leftBottomValue ~rightBottomValue =
  let topLeft={x=left;y=top;z=leftTopValue} in
  let topRight={x=right;y=top;z=rightTopValue} in
  let bottomLeft={x=left;y=bottom;z=leftBottomValue} in
  let bottomRight={x=right;y=bottom;z=rightBottomValue} in
  let valueQuad={topLeft;topRight;bottomLeft;bottomRight} in
  getIsoSegments quota valueQuad
  
 let computeIsoLine ~quota ~l ~t ~dx ~dy ~nx ~ny ~values =
        let result = ref [] in
		for y = 0 to ny - 2 do
		   let offset=y*nx in
		   let top = t -. (dy *. (float_of_int y)) in
		   let bottom =top -. dy in
			   
           for x = 0 to nx - 2 do
		       let left=l +. (dx *. (float_of_int x)) in
			   let right= left +. dx in
			   
			   let xOffset=offset+x in
			   
			   let leftTopValue=values.(xOffset) in
			   let rightTopValue= values.(xOffset+1)in
			   let leftBottomValue=values.(xOffset+nx) in
			   let rightBottomValue=values.(xOffset+nx+1) in
			   
			   let isoSegments=computeIsoSegments ~quota ~left ~top  ~right ~bottom ~leftTopValue ~rightTopValue ~leftBottomValue ~rightBottomValue in
			   
			   match isoSegments with
			   []-> ()
			   | [s0] -> result := s0::!result;
			   | s0::s1::[] -> result := s0::s1::!result;
			   | _ -> ()
		   done;
        done;
		
		!result
let last = function
  | x::xs -> List.fold_left (fun _ y -> y) x xs
  | []    -> failwith "no element"

let rec last = function
  | x::[] -> x
  | _::xs -> last xs
  | []    -> failwith "no element"

let remove_elt e l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | x::xs when e = x -> go xs acc
    | x::xs -> go xs (x::acc)
  in go l []

let remove_duplicates l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | x :: xs -> go (remove_elt x xs) (x::acc)
  in go l []

let remove_consecutive_duplicates l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | [x] -> go [] (x::acc)
    | x::y::tl -> if (x=y) then
			    go tl (x::acc)
			  else
			    go (y::tl) (x::acc)
  in go l []



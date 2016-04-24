
type t

val create: x:float -> y:float -> z:float -> t

val getX: t -> float
val getY: t -> float
val getZ: t -> float

val add: t -> t -> t
val sub: t -> t -> t
val mult: t -> float -> t

val getPointFromLine: p0:t -> p1:t -> d:float -> t list
    
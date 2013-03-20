
class TimeLike t where
  t0 :: t
  tnext :: t -> t 


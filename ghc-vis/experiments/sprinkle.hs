import Data.List


sprinkle :: a -> [a] -> [[a]]
sprinkle w xs = aux xs []
  where
    xs ## ys = foldl' (flip (:)) ys xs
    aux [] ys = [ys ## [w]]
    aux (x:xs) ys = (ys ## (w:x:xs)):(aux xs (x:ys))

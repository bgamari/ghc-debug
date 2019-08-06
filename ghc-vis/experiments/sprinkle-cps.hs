sprinkleCps :: a -> [a] -> [[a]]
sprinkleCps w xs = (w:xs):go id xs
  where
    go fxs [] = []
  	go fxs (y:ys) = fxs (y:w:ys) : go (fxs . (y:)) ys

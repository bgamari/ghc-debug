import GHC.Vis

main = do
  let a = "teeest"
  let b = [1..3]
  let c = b ++ b
  let d = [1..]
  putStrLn $ show $ d !! 1
{-
  vis
  putStrLn "Load the interface"
  getChar
  view a "a"
  view b "b"
  view c "c"
  view d "d"
  getChar
  switch
  getChar
  -}

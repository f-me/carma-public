import           Carma.EraGlonass.RequestId


main :: IO ()
main = do
  putStrLn "testing…"
  x <- newRequestId
  y <- newRequestId
  z <- newRequestId
  print x
  print y
  print z

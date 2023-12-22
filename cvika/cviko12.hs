main0 :: IO ()
main0 = putStrLn "Hello FUN!"

-- getLine :: IO String

main1 = do
  s <- getLine
  putStrLn ("Zadal si: " ++ s)

main2 = do
  putStr "Daj mi meno: "
  meno <- getLine
  putStr "Daj mi priezvisko: "
  priezvisko <- getLine
  putStrLn ("Volas sa " ++ meno ++ " " ++ priezvisko)

main3 = do
  subor <- readFile "subor.txt"
  putStrLn ("subor je: " ++ subor)
  
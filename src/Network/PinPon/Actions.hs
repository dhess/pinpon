module Network.PinPon.Actions (greet, sup, ciao, goodbye) where

greet :: String -> String -> IO ()
greet greeting target = putStrLn $ greeting ++ ", " ++ target

sup :: String -> IO ()
sup target = putStrLn $ "'Sup, " ++ target

ciao :: IO ()
ciao = putStrLn "Ciao!"

goodbye :: String -> String -> IO ()
goodbye signoff target = putStrLn $ signoff ++ ", " ++ target ++ ", it's been nice knowing you!"

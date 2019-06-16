module Main


runMain : String -> IO ()
runMain path = putStrLn path

main : IO ()
main = do args <- getArgs
          case args of
            (app::path::xs) => runMain path
            _ => putStrLn "Please provide path to the Json files"


testDir : IO ()
testDir = do (Right dir) <- dirOpen "./ala" | (Left err) => putStrLn "Wrong path"
             (Right e1) <- dirEntry dir
             putStrLn e1
             (Right e2) <- dirEntry dir
             putStrLn e2
             (Right e3) <- dirEntry dir
             putStrLn e3

-- import System.Directory
listDir : String -> IO (Either FileError (List String))
listDir path = do (Right dir) <- dirOpen path | (Left err) => pure (Left err)
                  fileNames <- listDir' dir
                  dirClose dir
                  pure (Right fileNames)
    where
      listDir' : Directory -> IO (List String)
      listDir' dir = do (Right fname) <- dirEntry dir | _ => pure []
                        rest <- listDir' dir
                        pure (fname :: rest)

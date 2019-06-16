module Main

-- import System.Directory


loadRecords : String -> IO (Either FileError (List String))
loadRecords path = do (Right fh) <- openFile path Read | Left err => pure (Left err)
                      rs <- loadRecords' fh
                      closeFile fh
                      pure $ Right rs
  where
    loadRecords2 : File -> IO (List String)
    loadRecords2 handle = do Right line <- fGetLine handle | pure []
                             putStrLn line
                             pure (line :: [])
    loadRecords' : File -> IO (List String)
    loadRecords' handle = do Right line <- fGetLine handle | pure []
                             putStrLn line
                             rs <- loadRecords2 handle
                             pure (line :: rs)


main : IO ()
main = do [_, path] <- getArgs | putStrLn "Please provide path to the Json files"
          Right rs <- loadRecords path
          putStrLn (fromMaybe "" (head' rs))

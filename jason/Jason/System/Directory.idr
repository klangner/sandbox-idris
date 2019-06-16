module Directory


||| Return list of all entries in a given directory
|||
||| ```idris
||| listDir "."
||| ```
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

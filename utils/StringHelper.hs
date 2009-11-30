types = ["BS.ByteString", "BL.ByteString", "ST.Text", "LT.Text"]
fromStringAttempts = ["Day", "Bool", "Int", "(Ratio Integer)"]
fromStringSuccesses = []
toStringAttempts = []
toStringSuccesses = ["Day", "Bool", "Int", "(Ratio Integer)"]

main = do
    mapM_ fsa $ fromStringAttempts ++ fromStringSuccesses
    mapM_ fss fromStringSuccesses
    mapM_ tsa $ toStringAttempts ++ toStringSuccesses
    mapM_ tss toStringSuccesses

fsa t = forM_ types (\f -> putStrLn $
    "instance ConvertAttempt " ++ f ++ " " ++ t ++ " where\n" ++
    "    convertAttempt = fromStringA <=< convertAttempt")
fss t = forM_ types (\f -> putStrLn $
    "instance ConvertSuccess " ++ f ++ " " ++ t ++ " where\n" ++
    "    convertSuccess = fromString . convertSuccess")
tsa f = forM_ types (\t -> putStrLn $
    "instance ConvertAttempt " ++ f ++ " " ++ t ++ " where\n" ++
    "    convertAttempt = convertAttempt . toString")
tss f = forM_ types (\t -> putStrLn $
    "instance ConvertSuccess " ++ f ++ " " ++ t ++ " where\n" ++
    "    convertSuccess = convertSuccess . toString")

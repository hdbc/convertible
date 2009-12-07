types = ["[Char]", "BS.ByteString", "BL.ByteString", "TS.Text", "TL.Text"]

pairs = do
    from <- types
    to <- types
    return (from, to)

main = do
    mapM_ cs pairs

cs (f, t) = putStrLn $ "instance ConvertSuccess " ++ f ++ " " ++ t ++ " where\n    convertSuccess = "

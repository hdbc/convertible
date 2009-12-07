import Control.Monad (guard)

fp = ["Double", "Float", "Rational"]
int = ["Int", "Int8", "Int16", "Int32", "Int64", "Word", "Word8", "Word16",
       "Word32", "Word64"]

intToIntWork :: [(String, String)]
intToIntWork = do
    i1 <- int
    i2 <- int
    guard $ i1 /= i2
    return (i1, i2)

showIntToInt :: (String, String) -> String
showIntToInt (src, dst) =
    "instance ConvertAttempt " ++ src ++ " " ++ dst ++ " where\n\
    \    convertAttempt = boundedConversion (return . fromIntegral)\n"

showIntToInteger :: String -> String
showIntToInteger i =
    "instance ConvertAttempt Integer " ++ i ++ " where\n\
    \    convertAttempt = boundedConversion (return . fromIntegral)\n\
    \instance ConvertSuccess " ++ i ++ " Integer where\n\
    \    convertSuccess = fromIntegral\n"

floatToIntWork :: [(String, String)]
floatToIntWork = do
    f <- fp
    i <- int
    return (f, i)

showFloatToInt :: (String, String) -> String
showFloatToInt (f, i) =
    "instance ConvertAttempt " ++ f ++ " " ++ i ++ " where\n\
    \    convertAttempt = boundedConversion (return . truncate)\n\
    \instance ConvertSuccess " ++ i ++ " " ++ f ++ " where\n\
    \    convertSuccess = fromIntegral\n"

floatToFloatWork :: [(String, String)]
floatToFloatWork = do
    f1 <- fp
    f2 <- fp
    guard $ f1 /= f2
    return (f1, f2)

showFloatToFloat :: (String, String) -> String
showFloatToFloat (src, dst) =
    "instance ConvertSuccess " ++ src ++ " " ++ dst ++ " where\n\
    \    convertSuccess = " ++ convertFunc ++ "\n"
    where
        convertFunc
            | dst == "Rational" = "toRational"
            | otherwise = "realToFrac"

showFloatToInteger f =
    "instance ConvertSuccess Integer " ++ f ++ " where\n\
    \    convertSuccess = fromIntegral\n\
    \instance ConvertSuccess " ++ f ++ " Integer where\n\
    \    convertSuccess = truncate\n"

showIntToChar i =
    "instance ConvertAttempt Char " ++ i ++ " where\n\
    \    convertAttempt = boundedConversion (return . fromIntegral . fromEnum)\n\
    \instance ConvertAttempt " ++ i ++ " Char where\n\
    \    convertAttempt = boundedConversion (return . toEnum . fromIntegral)\n"

main = do
    mapM_ (putStrLn . showIntToInteger) int
    mapM_ (putStrLn . showIntToInt) intToIntWork
    mapM_ (putStrLn . showFloatToInteger) fp
    mapM_ (putStrLn . showFloatToInt) floatToIntWork
    mapM_ (putStrLn . showFloatToFloat) floatToFloatWork
    mapM_ (putStrLn . showIntToChar) int

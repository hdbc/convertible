cint = ["CChar", "CSChar", "CUChar", "CShort", "CUShort", "CInt", "CUInt", "CLong",
        "CULong", "CSize", "CWchar", "CLLong", "CULLong"]
cfloat = ["CFloat", "CDouble", "CLDouble"]
hsint = ["Int", "Int8", "Int16", "Int32", "Int64", "Word", "Word8", "Word16", "Word32",
         "Word64"]
hsfloat = ["Double", "Float", "Rational"]

printFP (f, i) =
    "instance ConvertAttempt " ++ f ++ " " ++ i ++ " where \n\
    \    convertAttempt = boundedConversion (return . truncate)\n\
    \instance ConvertAttempt " ++ i ++ " " ++ f ++ " where \n\
    \    convertAttempt= return . fromIntegral\n\
    \instance ConvertSuccess " ++ i ++ " " ++ f ++ " where \n\
    \    convertSuccess= fromIntegral\n"

printIntegerF f =
    "instance ConvertAttempt " ++ f ++ " Integer where\n\
    \    convertAttempt = return . truncate\n\
    \instance ConvertSuccess " ++ f ++ " Integer where\n\
    \    convertSuccess = truncate\n\
    \instance ConvertAttempt Integer " ++ f ++ " where\n\
    \    convertAttempt = return . fromIntegral\n\
    \instance ConvertSuccess Integer " ++ f ++ " where\n\
    \    convertSuccess = fromIntegral\n"

printIntegerI i =
    "instance ConvertAttempt " ++ i ++ " Integer where\n\
    \    convertAttempt = return . fromIntegral\n\
    \instance ConvertSuccess " ++ i ++ " Integer where\n\
    \    convertSuccess = fromIntegral\n\
    \instance ConvertAttempt Integer " ++ i ++ " where\n\
    \    convertAttempt = boundedConversion (return . fromIntegral)\n"

printCharI i =
    "instance ConvertAttempt " ++ i ++ " Char where\n\
    \    convertAttempt = boundedConversion (return . toEnum . fromIntegral)\n\
    \instance ConvertAttempt Char " ++ i ++ " where\n\
    \    convertAttempt = boundedConversion (return . fromIntegral . fromEnum)\n"

printFP1 (f1, f2) = 
    "instance ConvertAttempt " ++ f1 ++ " " ++ f2 ++ " where\n\
    \    convertAttempt = return . realToFrac\n\
    \instance ConvertSuccess " ++ f1 ++ " " ++ f2 ++ " where\n\
    \    convertSuccess = realToFrac\n"

printFPFP (f1, f2) = printFP1 (f1, f2) ++ printFP1 (f2, f1)

printInt (i1, i2) =
    "instance ConvertAttempt " ++ i1 ++ " " ++ i2 ++ " where\n\
    \    convertAttempt = boundedConversion (return . fromIntegral)\n"

printIntInt (i1, i2) = printInt (i1, i2) ++ printInt (i2, i1)

main = do putStrLn "-- Section 1"
          mapM_ (putStrLn . printFP) (concatMap (\x -> map (\y -> (x, y)) hsint) cfloat)
          putStrLn "-- Section 2"
          mapM_ (putStrLn . printFPFP) (concatMap (\x -> map (\y -> (x, y)) hsfloat) cfloat)
          putStrLn "-- Section 3"
          mapM_ (putStrLn . printIntInt) (concatMap (\x -> map (\y -> (x, y)) hsint) cint)
          putStrLn "-- Section 4"
          mapM_ (putStrLn . printInt) . filter (\(a, b) -> a /= b) $ 
                (concatMap (\x -> map (\y -> (x, y)) cint) cint)
          putStrLn "-- Section 5"
          mapM_ (putStrLn . printFP1) . filter (\(a, b) -> a /= b) $
                (concatMap (\x -> map (\y -> (x, y)) cfloat) cfloat)
          putStrLn "-- Section 6"
          mapM_ (putStrLn . printIntegerF) cfloat
          putStrLn "-- Section 7"
          mapM_ (putStrLn . printIntegerI) cint
          putStrLn "-- Section 8o"
          mapM_ (putStrLn . printCharI) cint

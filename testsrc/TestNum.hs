{-# LANGUAGE
    ScopedTypeVariables
  , FlexibleContexts #-}

{-
Copyright (C) 2009-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}
module TestNum where

import Data.Convertible

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Arbitrary(..), Gen)
import Test.QuickCheck.Assertions
import Test.QuickCheck.Property
import Data.Decimal
import Control.Applicative

import Data.Typeable
import Data.Word
import Data.Int

#if MIN_VERSION_Decimal(0,2,4)
-- Decimal-0.2.4 has no Arbitrary instance in library any more
instance (Arbitrary i, Integral i) => Arbitrary (DecimalRaw i) where
  arbitrary = Decimal <$> arbitrary <*> arbitrary
#endif

propFromToBounded :: (Convertible a b, Convertible b a, Eq a, Ord a, Show a, Num a, Integral a, Bounded a, Integral b, Bounded b)
                     => a -- ^ value to convert
                     -> b -- ^ fantom variable carrying type to convert to
                     -> Property
propFromToBounded a b = (a >= minb) && (a <= maxb) ==>
                        a ==? (convert (asTypeOf (convert a) b))
  where
    (minb, maxb) = getBounds a b

propFromToBoundedInt :: (Convertible Integer b, Convertible b Integer, Integral b, Bounded b)
                     => Integer -- ^ value to convert
                     -> b       -- ^ fantom variable carrying type to convert to
                     -> Property
propFromToBoundedInt a b = (a >= minb) && (a <= maxb) ==>
                        a ==? (convert (asTypeOf (convert a) b))
  where
    minb = fromIntegral $ asTypeOf minBound b
    maxb = fromIntegral $ asTypeOf maxBound b

    
propFromToBoundedFailed :: forall a b. (Convertible a b, Convertible b a, Eq a, Ord a, Show a, Num a, Integral a, Bounded a, Typeable a, Integral b, Bounded b, Typeable b)
                           => a -- ^ value to test
                           -> b -- ^ fantom variable carrying type to convert to
                           -> Property
propFromToBoundedFailed a b = (a < minb) || (a > maxb) ==>
                              case res of
                                Right x -> failed {reason = "value " ++ show a ++ " converted to " ++ show x ++ " but should fail a:: " ++ (show $ typeOf a) ++ ", b :: " ++ (show $ typeOf b)}
                                Left _  -> succeeded
  where
    res :: ConvertResult a
    res = do
      r <- safeConvert a :: (ConvertResult b)
      safeConvert r
    (minb, maxb) = getBounds a b

propFromToBoundedFailedInt :: forall b. (Convertible Integer b, Convertible b Integer, Integral b, Bounded b, Typeable b)
                           => Integer -- ^ value to test
                           -> b       -- ^ fantom variable carrying type to convert to
                           -> Property
propFromToBoundedFailedInt a b = (a < minb) || (a > maxb) ==>
                              case res of
                                Right x -> failed {reason = "value " ++ show a ++ " converted to " ++ show x ++ " but should fail a:: " ++ (show $ typeOf a) ++ ", b :: " ++ (show $ typeOf b)}
                                Left _  -> succeeded
  where
    res :: ConvertResult Integer
    res = do
      r <- safeConvert a :: (ConvertResult b)
      safeConvert r
    minb = fromIntegral $ asTypeOf minBound b
    maxb = fromIntegral $ asTypeOf maxBound b
    

propFromTo :: (Convertible a b, Convertible b a, Eq a, Show a) => a -> b -> Result
propFromTo a b = a ==? (convert (asTypeOf (convert a) b))


outOfBoundsInt :: (Bounded b, Integral b) => b -> Gen Integer
outOfBoundsInt b = do
  a <- arbitrary
  return $ if a > 0
           then a + maxb
           else a - minb - 1
  where
    maxb = fromIntegral $ asTypeOf maxBound b
    minb = fromIntegral $ asTypeOf minBound b

insideBoundsInt :: (Bounded b, Integral b) => b -> Gen Integer
insideBoundsInt b = do
  a <- arbitrary
  return $ minb + (a `mod` (maxb - minb))
  where
    maxb = fromIntegral $ asTypeOf maxBound b
    minb = fromIntegral $ asTypeOf minBound b

checkInsideBounds :: forall a b. (Convertible a b, Convertible b a, Eq a, Ord a, Show a, Num a, Integral a, Bounded a, Arbitrary a, Integral b, Bounded b) => a -> b -> Property
checkInsideBounds _ b = forAll (insideBounds b) $ \(x :: a) -> propFromToBounded x b

checkInsideBoundsInt :: (Convertible Integer b, Convertible b Integer, Integral b, Bounded b) => b -> Property
checkInsideBoundsInt b = forAll (insideBoundsInt b) $ \(x :: Integer) -> propFromToBoundedInt x b
                                                               

checkOutsideBounds :: forall a b. (Convertible a b, Convertible b a, Eq a, Ord a, Show a, Num a, Arbitrary a, Integral a, Bounded a, Typeable a, Integral b, Bounded b, Typeable b) => a -> b -> Property
checkOutsideBounds _ b = forAll (outOfBounds b) $ \(x :: a) -> propFromToBoundedFailed x b

checkOutsideBoundsInt :: (Convertible Integer b, Convertible b Integer, Integral b, Bounded b, Typeable b) => b -> Property
checkOutsideBoundsInt b = forAll (outOfBoundsInt b) $ \(x :: Integer) -> propFromToBoundedFailedInt x b
                                                               
checkBoth :: (Convertible a b, Convertible b a, Eq a, Ord a, Show a, Num a, Arbitrary a, Typeable a, Integral a, Bounded a, Integral b, Bounded b, Typeable b) => a -> b -> Property
checkBoth a b = (checkInsideBounds a b) .&&. (checkOutsideBounds a b)
                
checkBothInt :: (Convertible Integer b, Convertible b Integer, Integral b, Bounded b, Typeable b) => b -> Property
checkBothInt b = (checkInsideBoundsInt b) .&&. (checkOutsideBoundsInt b)

getBounds :: (Bounded a, Bounded b, Integral a, Integral b) => a -> b -> (a, a)
getBounds a b = (minx, maxx)
  where
    mina :: Integer
    mina = fromIntegral $ asTypeOf minBound a
    maxa = fromIntegral $ asTypeOf maxBound a
    minb = fromIntegral $ asTypeOf minBound b
    maxb = fromIntegral $ asTypeOf maxBound b
    minx = fromInteger $ max mina minb
    maxx = fromInteger $ min maxa maxb
    
outOfBounds :: (Bounded b, Integral b, Bounded a, Integral a, Num a, Arbitrary a, Ord a) => b -> Gen a
outOfBounds b = do
  a <- arbitrary
  let (minb, maxb) = getBounds a b
  return $ if a > 0
           then a + maxb
           else a - minb - 1
  
insideBounds :: (Bounded b, Integral b, Bounded a, Integral a, Num a, Arbitrary a, Ord a) => b -> Gen a
insideBounds b = do
  a <- arbitrary
  let (minb, maxb) = getBounds a b
  return $ minb + (a `mod` (maxb - minb))

                                                               
allt :: Spec
allt = describe "Numeric tests" $ do
  prop "Integer -> Int" $ checkBothInt (undefined :: Int)
  prop "Integer -> Int8" $ checkBothInt (undefined :: Int8)
  prop "Integer -> Int16" $ checkBothInt (undefined :: Int16)
  prop "Integer -> Int32" $ checkBothInt (undefined :: Int32)
  prop "Integer -> Int64" $ checkBothInt (undefined :: Int64)

  prop "Integer -> Word" $ checkBothInt (undefined :: Word)
  prop "Integer -> Word8" $ checkBothInt (undefined :: Word8)
  prop "Integer -> Word16" $ checkBothInt (undefined :: Word16)
  prop "Integer -> Word32" $ checkBothInt (undefined :: Word32)
  prop "Integer -> Word64" $ checkBothInt  (undefined :: Word64)

  prop "Int64 -> Int8" $ checkBoth (undefined :: Int64) (undefined :: Int8)
  prop "Int64 -> Int16" $ checkBoth (undefined :: Int64) (undefined :: Int16)
  prop "Int64 -> Int32" $ checkBoth (undefined :: Int64) (undefined :: Int32)

  prop "Int64 -> Word8" $ checkBoth (undefined :: Int64) (undefined :: Word8)
  prop "Int64 -> Word16" $ checkBoth (undefined :: Int64) (undefined :: Word16)
  prop "Int64 -> Word32" $ checkBoth (undefined :: Int64) (undefined :: Word32)
  prop "Int64 -> Word64" $ checkBoth (undefined :: Int64) (undefined :: Word64)

  prop "Int32 -> Int8" $ checkBoth (undefined :: Int32) (undefined :: Int8)
  prop "Int32 -> Int16" $ checkBoth (undefined :: Int32) (undefined :: Int16)

  prop "Int32 -> Word8" $ checkBoth (undefined :: Int32) (undefined :: Word8)
  prop "Int32 -> Word16" $ checkBoth (undefined :: Int32) (undefined :: Word16)
  prop "Int32 -> Word32" $ checkBoth (undefined :: Int32) (undefined :: Word32)

  prop "Int16 -> Int8" $ checkBoth (undefined :: Int16) (undefined :: Int8)

  prop "Int16 -> Word8" $ checkBoth (undefined :: Int16) (undefined :: Word8)
  prop "Int16 -> Word16" $ checkBoth (undefined :: Int16) (undefined :: Word16)

  prop "Word64 -> Int8" $ checkBoth (undefined :: Word64) (undefined :: Int8)
  prop "Word64 -> Int16" $ checkBoth (undefined :: Word64) (undefined :: Int16)
  prop "Word64 -> Int32" $ checkBoth (undefined :: Word64) (undefined :: Int32)
  prop "Word64 -> Int64" $ checkBoth (undefined :: Word64) (undefined :: Int64)

  prop "Word64 -> Word8" $ checkBoth (undefined :: Word64) (undefined :: Word8)
  prop "Word64 -> Word16" $ checkBoth (undefined :: Word64) (undefined :: Word16)
  prop "Word64 -> Word32" $ checkBoth (undefined :: Word64) (undefined :: Word32)

  prop "Word32 -> Int8" $ checkBoth (undefined :: Word32) (undefined :: Int8)
  prop "Word32 -> Int16" $ checkBoth (undefined :: Word32) (undefined :: Int16)
  prop "Word32 -> Int32" $ checkBoth (undefined :: Word32) (undefined :: Int32)

  prop "Word32 -> Word8" $ checkBoth (undefined :: Word32) (undefined :: Word8)
  prop "Word32 -> Word16" $ checkBoth (undefined :: Word32) (undefined :: Word16)

  prop "Word16 -> Int8" $ checkBoth (undefined :: Word16) (undefined :: Int8)
  prop "Word16 -> Int16" $ checkBoth (undefined :: Word16) (undefined :: Int16)

  prop "Word16 -> Word8" $ checkBoth (undefined :: Word16) (undefined :: Word8)

  prop "Word8 -> Int8" $ checkBoth (undefined :: Word8) (undefined :: Int8)

  prop "Int8 -> Double" $ \(a :: Int8) -> propFromTo a (undefined :: Double)
  prop "Int16 -> Double" $ \(a :: Int16) -> propFromTo a (undefined :: Double)
  prop "Int32 -> Double" $ \(a :: Int32) -> propFromTo a (undefined :: Double)

  prop "Word8 -> Double" $ \(a :: Word8) -> propFromTo a (undefined :: Double)
  prop "Word16 -> Double" $ \(a :: Word16) -> propFromTo a (undefined :: Double)
  prop "Word32 -> Double" $ \(a :: Word32) -> propFromTo a (undefined :: Double)

  prop "Int8 -> Float" $ \(a :: Int8) -> propFromTo a (undefined :: Float)
  prop "Int16 -> Float" $ \(a :: Int16) -> propFromTo a (undefined :: Float)

  prop "Word8 -> Float" $ \(a :: Word8) -> propFromTo a (undefined :: Float)
  prop "Word16 -> Float" $ \(a :: Word16) -> propFromTo a (undefined :: Float)

  prop "Int8 -> Decimal" $ \(a :: Int8) -> propFromTo a (undefined :: Decimal)
  prop "Int16 -> Decimal" $ \(a :: Int16) -> propFromTo a (undefined :: Decimal)
  prop "Int32 -> Decimal" $ \(a :: Int32) -> propFromTo a (undefined :: Decimal)
  prop "Int64 -> Decimal" $ \(a :: Int64) -> propFromTo a (undefined :: Decimal)
  prop "Int -> Decimal" $ \(a :: Int) -> propFromTo a (undefined :: Decimal)

  prop "Word8 -> Decimal" $ \(a :: Word8) -> propFromTo a (undefined :: Decimal)
  prop "Word16 -> Decimal" $ \(a :: Word16) -> propFromTo a (undefined :: Decimal)
  prop "Word32 -> Decimal" $ \(a :: Word32) -> propFromTo a (undefined :: Decimal)
  prop "Word64 -> Decimal" $ \(a :: Word64) -> propFromTo a (undefined :: Decimal)
  prop "Word -> Decimal" $ \(a :: Word) -> propFromTo a (undefined :: Decimal)
  
  prop "Int8 -> Rational" $ \(a :: Int8) -> propFromTo a (undefined :: Rational)
  prop "Int16 -> Rational" $ \(a :: Int16) -> propFromTo a (undefined :: Rational)
  prop "Int32 -> Rational" $ \(a :: Int32) -> propFromTo a (undefined :: Rational)
  prop "Int64 -> Rational" $ \(a :: Int64) -> propFromTo a (undefined :: Rational)
  prop "Int -> Rational" $ \(a :: Int) -> propFromTo a (undefined :: Rational)

  prop "Word8 -> Rational" $ \(a :: Word8) -> propFromTo a (undefined :: Rational)
  prop "Word16 -> Rational" $ \(a :: Word16) -> propFromTo a (undefined :: Rational)
  prop "Word32 -> Rational" $ \(a :: Word32) -> propFromTo a (undefined :: Rational)
  prop "Word64 -> Rational" $ \(a :: Word64) -> propFromTo a (undefined :: Rational)
  prop "Word -> Rational" $ \(a :: Word) -> propFromTo a (undefined :: Rational)

  prop "Float -> Double" $ \(a :: Float) -> propFromTo a (undefined :: Double)
  
  prop "Float -> Decimal" $ \(a :: Float) -> propFromTo a (undefined :: Decimal)
  prop "Double -> Decimal" $ \(a :: Double) -> propFromTo a (undefined :: Decimal)
  
  prop "Float -> Rational" $ \(a :: Float) -> propFromTo a (undefined :: Rational)
  prop "Double -> Rational" $ \(a :: Double) -> propFromTo a (undefined :: Rational)
  prop "Decimal -> Rational" $ \(a :: Decimal) -> propFromTo a (undefined :: Rational)
  prop "Char -> Integer" $ \(a :: Char) -> propFromTo a (undefined :: Integer)
  prop "Char -> Int" $ \(a :: Char) -> propFromTo a (undefined :: Int)
  prop "Char -> Int16" $ \(a :: Char) -> propFromTo a (undefined :: Int16)
  prop "Char -> Int32" $ \(a :: Char) -> propFromTo a (undefined :: Int32)
  prop "Char -> Int64" $ \(a :: Char) -> propFromTo a (undefined :: Int64)
  prop "Char -> Word" $ \(a :: Char) -> propFromTo a (undefined :: Word)
  prop "Char -> Word8" $ \(a :: Char) -> propFromTo a (undefined :: Word)
  prop "Char -> Word16" $ \(a :: Char) -> propFromTo a (undefined :: Word16)
  prop "Char -> Word32" $ \(a :: Char) -> propFromTo a (undefined :: Word32)
  prop "Char -> Word64" $ \(a :: Char) -> propFromTo a (undefined :: Word64)

-- ---------------------------------------------------------------- [ Main.idr ]
-- Module      : Main
-- Description : This code is a simple example of using Java APIs,
--               in this case to parse a float from a string and to 
--               convert the resulting value to its underlying bit
--               representation.
--               The program was designed for my first year students to
--               be able to get a basic understanding of how IEEE 754
--               numbers are stored internally within a machine.
--
-- This code is distributed under the BSD 2-clause license.
-- See the file LICENSE in the root directory for its full text.
--
-- --------------------------------------------------------------------- [ EOH ]
module Main

import IdrisJava
import IdrisJava.Interactive

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings
import public Data.Bits

--------------------------------------------------------------------------------
-- Java FFI calls

FloatToIntBits : Type
FloatToIntBits = Float ->  JAVA_IO Int

floatToIntBits : FloatToIntBits
floatToIntBits value  =  invoke "Float.floatToIntBits" FloatToIntBits value

IntBitsToFloat : Type
IntBitsToFloat = Int ->  JAVA_IO Float

intBitsToFloat : IntBitsToFloat
intBitsToFloat bits  =  invoke "Float.intBitsToFloat" IntBitsToFloat bits

ParseFloat : Type
ParseFloat = String ->  JAVA_IO Float

parseFloat : ParseFloat
parseFloat s  =  invoke "Float.parseFloat" ParseFloat s

--------------------------------------------------------------------------------
-- Handle bit patterns for sing, exponent, and mantissa

-- bit pattern to extract mantissa
b00000000011111111111111111111111 : Integer
b00000000011111111111111111111111 = 8388607

bMantissa : Bits 32
bMantissa = intToBits b00000000011111111111111111111111

extractMantissa : Bits 32 -> Bits 32
extractMantissa bits = bits `and` bMantissa

-- bit patten to extract exponent
b01111111100000000000000000000000 : Integer
b01111111100000000000000000000000 = 2139095040

bExponent : Bits 32
bExponent = intToBits b01111111100000000000000000000000

extractExponent : Bits 32 -> Bits 32
extractExponent bits = (bits `and` bExponent) `shiftRightLogical` (intToBits 23)

mantissiaToFloat : Bits 32 -> Float
mantissiaToFloat bits = 
  fromInteger $ bitsToInt $ (((intToBits 127) `shiftLeft` (intToBits 23)) `or` bits)

bitsToList : Bits n -> List Char
bitsToList x = helper last x
    where
      %assert_total
      helper : Fin (S n) -> Bits n -> List Char
      helper FZ _ = []
      helper (FS x) b = (if getBit x b then '1' else '0') :: helper (weaken x) b
    
-- seperate in (sign, exponent, mantissa)
%default partial
breakUp : Bits 32 -> (Char, List Char, List Char)
breakUp bits = let lsbits = bitsToList bits
                   (Just sign, afterSign)    = (head' lsbits, drop 1 lsbits)
                   (exponent, mantissa) = (take 8 afterSign, drop 8 afterSign)
               in (sign, exponent, mantissa)

--------------------------------------------------------------------------------
-- Display formated output
-- TODO: @bgaster - bug with generating to large Java methods for JVM!
--                  workaround is just to split functions

displayEncoded : Char -> Bits 32 -> Bits 32 -> JAVA_IO ()
displayEncoded sign exponent mantissa = 
    putStrLn ("Encoded as:       " ++ (if sign == '1' then "1" else "0") ++
               "              " ++ show (bitsToInt exponent) ++
               "                          " ++ show mantissa) --(bitsToInt mantissa))

displayValue : Char -> Bits 32 -> Bits 32 -> JAVA_IO ()
displayValue sign exponent mantissa = 
     putStrLn ("Value             " ++  (if sign == '1' then "-1" else "+1") ++
               "             2^" ++ show ((bitsToInt exponent) - 127) ++
               "                      " ++ show (mantissiaToFloat mantissa)) *>
     displayEncoded sign exponent mantissa

displayDetails : Bits 32 -> JAVA_IO ()
displayDetails bits = 
  let mantissa = extractMantissa bits
      exponent = extractExponent bits
      (sign, exponent', mantissa') = breakUp bits
  in putStrLn "                 Sign          Exponent                      Mantissa" *>
     displayValue sign exponent mantissa
        
display : String -> Bits 32 -> Float -> JAVA_IO ()
display input bits ieee = 
  do displayDetails bits
     putStrLn ("Decimial representation: " ++ pack (pad 14) ++ input ++ "\n" ++
               "Binary representation: " ++ pack (pad 16) ++ show bits ++ "\n" ++
               "Floating point representation in Java: " ++ show ieee)
  where
    mk : List Char -> String
    mk ls = pack ls
     
    pad : Nat -> List Char
    pad Z     = Nil
    pad (S k) = ' ' :: pad k

--------------------------------------------------------------------------------
-- Handle command line arguments

usage : JAVA_IO ()
usage = putStrLn "usage: " *>
        putStrLn "  ieeeconverter floating-point-number"
        
processArg : JAVA_IO (Maybe String)
processArg = case !getArgs of
             [_, value] => pure $ Just value
             _          => pure $ Nothing

--------------------------------------------------------------------------------
-- Main

main : JAVA_IO ()
main = do (Just value) <- processArg
             | _       => usage
          f    <- parseFloat value
          ieee <- floatToIntBits f
          let bits = intToBits $ cast ieee
          backAgain <- intBitsToFloat ieee
          display value bits backAgain

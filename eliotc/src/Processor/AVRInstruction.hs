{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE BinaryLiterals #-}
{-| AVR instructions and generating bytes.
 -}

module Processor.AVRInstruction (Register(..), Instruction(..), instructionsToBytes) where

import Data.ByteString
import Data.Word
import Data.Bits

data Register = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 | R16 | R17 | R18 | R19 | R20 | R21 | R22 | R23 | R24 | R25 | R26 | R27 | R28 | R29 | R30 | R31 | R32
   deriving (Eq, Show, Enum)

data Instruction = Push  Register
                 | Ldi   Register Word8
                 | Rcall Word16

instructionToBytes :: Instruction -> Either String ByteString
instructionToBytes (Push r)  = Right $ pack [0b10010010 + ((fromIntegral (fromEnum r)) `shiftR` 5), ((fromIntegral (fromEnum r)) `shiftL` 4) + 0b00001111]
instructionToBytes (Ldi r i)
   | fromEnum r < 16 = Left $ "Can not load immediate into " ++ (show r) ++ ", only R16-R32 can be used."
   | otherwise       = Right $ pack [0b11100000 + (i .&. 0x0F), (fromIntegral (((fromEnum r)-16)) `shiftL` 4) + (i `shiftR` 4)]
instructionToBytes (Rcall a)
   | a >= 4096       = Left $ "Relative call address (" ++ (show a) ++ ") out of bounds. Allowed 0 - 4095."
   | otherwise       = Right $ pack [0b11010000 + (fromIntegral (a `shiftR` 8)), fromIntegral (a .&. 0xFF)]

instructionsToBytes :: [Instruction] -> Either String ByteString
instructionsToBytes is = Data.ByteString.concat <$> mapM instructionToBytes is

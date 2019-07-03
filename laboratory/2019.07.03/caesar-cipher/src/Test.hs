module Test where

import Data.Char
import Test.QuickCheck
import Caesar

-- Aplicando shift duas vezes, uma com o valor negativo,
-- o caractere deve ser o mesmo.
prop_neg_shift :: Int -> Char -> Property
prop_neg_shift d c = isLetter c && isLower c
  ==> shift (-d) (shift d c) == c

-- O tamanho da mensagem codificada deve ser o mesmo da original.
prop_enc_length :: Int -> String -> Bool
prop_enc_length d s = length (encode d s) == length s

-- O decode do encode deve ser a String original.
prop_enc_dec :: Int -> String -> Bool
prop_enc_dec d s = crack (encode d s) == s

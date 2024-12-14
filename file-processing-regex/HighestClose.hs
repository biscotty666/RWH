{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Applicative (Alternative(empty))

{-
  L.readInt parses an in
-}
closing = readPrice . (!!1) . L.split ','

readPrice :: L.ByteString -> Maybe Int
readPrice str =
    case L.readInt str of
        Nothing     -> Nothing
        Just (dollars,rest) ->
            case L.readInt (L.tail rest) of
                Nothing     -> Nothing
                Just (cents, more) ->
                    Just (dollars * 100 + cents)

myNum :: L.ByteString
myNum = "123.15"

-- ghci> readPrice myNum
-- Just 12315

-- the use of (Nothing:) handles the case of an empty list
highestClose :: L.ByteString -> Maybe Int
highestClose = maximum . (Nothing:) . map closing . L.lines

highestCloseFrom path = do
    contents <- L.readFile path
    print (highestClose contents)

--  highestCloseFrom "aapl.csv"
--  Just 23648

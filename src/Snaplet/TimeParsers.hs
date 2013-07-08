module Snaplet.TimeParsers
    (parseTimes) where

import Text.Parsec
import Text.Parsec.Text
import Control.Applicative ((<$>),(*>),(<*))
import Data.Time
import Data.Time.Calendar
import Data.Time.LocalTime
import qualified Data.Text as T

import Snaplet.Types


-- | Parse times from datetime format resembling
-- ISO 8601: "@2013-07-31T12:34:56" in user's local timezone
-- (local timezone not implemented yet)
parse1Time :: Parser UTCTime
parse1Time = do
    year  <- at       >> pInt 4
    month <- hyphen   >> pInt 2 
    day   <- hyphen   >> pInt 2
    hh    <- char 'T' >> pInt 2
    mm    <- colon    >> pInt 2
    ss    <- colon    >> pInt 2
    
    case fromGregorianValid year month day of
        Just day -> case makeTimeOfDayValid hh mm (fromIntegral ss) of
            Just tod -> do
                let localTime = LocalTime day tod
                return $ localTimeToUTC tz localTime
            Nothing  -> parserZero
        _      -> parserZero
  where tz     = hoursToTimeZone 3 -- TODO: time zones per user
        at     = char '@'
        hyphen = char '-'
        colon  = char ':'
        pInt n = read <$> count n digit

parseTimes' :: Parser [UTCTime]
parseTimes' = do
    let single = manyTill anyChar (lookAhead parse1Time) *> parse1Time
    many single

parseTimes :: T.Text -> [UTCTime]
parseTimes text = do
    case parse parseTimes' "" text of
        Left _      -> []
        Right times -> times

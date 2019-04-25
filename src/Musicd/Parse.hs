module Musicd.Parse
  ( parse
  )
where

import ClassyPrelude hiding (try)
import Musicd.Types

import Data.Char (isSpace)
import Text.Read (read)

import Data.Void            (Void)
import Text.Megaparsec      hiding (many, some, parse)
import Text.Megaparsec.Char

type Parser = Parsec Void Text

-- | Parse the playlist file into a `PlaySpec`
parse :: Text -> PlaySpec
parse line = fromMaybe None (parseMaybe parser line)

parser :: Parser PlaySpec
parser = pause <|> youtube <|> random <|> glob <|> useList <|> command <|> file

file, youtube, command, random, glob, pause, useList :: Parser PlaySpec
file = File <$> some anySingle
pause = Pause <$ eof

youtube = YouTube <$> (char '@' *> many anySingle)

useList = UseList <$> (char '-' *> optional space *> many anySingle)

command = Command <$> (char ':' *> word) <*> many word
  where word = takeWhile1P Nothing (not . isSpace) <* space

random = char '?' *> (stream <|> try numRandom <|> oneRandom)
 where
  numRandom = mkRandom <$> some digitChar <* char '?' <*> some anySingle
  mkRandom x = Random (read x)
  oneRandom = Random 1 <$> some anySingle
  stream    = Stream <$> (char '?' *> some anySingle)

glob = Glob <$> (char '=' *> optional space *> some anySingle)

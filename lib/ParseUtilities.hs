module ParseUtilities (ProgramInfo(..), ProgramParser, readFields, lookupSF, insertField, insertSF, insertDep, insertSF2, emptyPI) where

import qualified Data.Map.Strict as Map
import qualified Data.MultiMap as MM
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P
import Text.ParserCombinators.Parsec
import Data.Either
import Prelude hiding (pi)

lexer :: P.TokenParser ()
lexer = P.makeTokenParser emptyDef

symbol :: String -> Parser String
symbol = P.symbol lexer

identifier :: Parser String
identifier = P.identifier lexer

data ProgramInfo = ProgramInfo
  { deps :: MM.MultiMap String String,
    subfields :: Map.Map String (Map.Map String String),
    current :: String,
    fields :: [String],
    currentFile :: String
  }

instance (Show ProgramInfo) where
  show pi = "ProgramInfo {\n" ++
    "  deps = " ++ show (MM.toMap (deps pi)) ++ ",\n"
    ++
    "  subfields = " ++ show (subfields pi) ++ ",\n"
    ++
    "  current = " ++ show (current pi) ++ ",\n"
    ++
    "  currentFile = " ++ show (currentFile pi) ++ ",\n"
    ++
    "}"

emptyPI :: ProgramInfo
emptyPI = ProgramInfo MM.empty Map.empty "" [] ""

insert2 :: (Ord a, Eq b) => a -> b -> MM.MultiMap a b -> MM.MultiMap a b
insert2 x y mm = if y `elem` MM.lookup x mm then mm else MM.insert x y mm

insertDep :: String -> ProgramInfo -> ProgramInfo
insertDep y pi = pi {deps = insert2 (current pi) y (deps pi)}

insertSF :: String -> String -> ProgramInfo -> ProgramInfo
insertSF name y pi =
  let currentMap = Map.lookup (current pi) (subfields pi)
   in case currentMap of
        Nothing -> pi {subfields = Map.insert (current pi) (Map.singleton name y) (subfields pi)}
        Just mp -> pi {subfields = Map.insert (current pi) (Map.insert name y mp) (subfields pi)}

insertSF2 :: String -> String -> String -> ProgramInfo -> ProgramInfo
insertSF2 lab name y pi =
  let currentMap = Map.lookup lab (subfields pi)
   in case currentMap of
        Nothing -> pi {subfields = Map.insert lab (Map.singleton name y) (subfields pi)}
        Just mp -> pi {subfields = Map.insert lab (Map.insert name y mp) (subfields pi)}

lookupSF :: String -> String -> ProgramInfo -> Maybe String
lookupSF propName field pi =
  case Map.lookup propName (subfields pi) of
    Nothing -> Nothing
    Just sfs -> Map.lookup field sfs

insertField :: String -> ProgramInfo -> ProgramInfo
insertField y pi = pi {fields = y : fields pi}

type ProgramParser = ProgramInfo -> Parser ProgramInfo

fieldsParser :: ProgramParser
fieldsParser pi =
  do eof; return pi
    <|> try
      ( do
          _ <- symbol "#"
          expr <- identifier
          fieldsParser (pi {current = expr})
          -- parses eol automatically?
      )
    <|> ( do
            expr <- many1 (noneOf "\n")
            fieldsParser (insertDep expr pi) -- does this preserve the order?
        )
    <|> do _ <- anyToken; fieldsParser pi

readFields :: String -> MM.MultiMap String String
readFields contents = deps $ fromRight (error "") (parse (fieldsParser emptyPI) "error" contents)

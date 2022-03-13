module Main (main) where

import System.Environment
import System.IO

import Data.Maybe
import Data.Graph.Inductive
import qualified Data.Map.Strict as M
import qualified Data.MultiMap as MM

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

import Text.LaTeX.Base
import Text.LaTeX.Base.Parser
import Text.LaTeX.Base.Syntax

import Utilities
import ParseUtilities
import DAGViz

lexer :: P.TokenParser ()
lexer  = P.makeTokenParser emptyDef

whiteSpace= P.whiteSpace lexer
lexeme    = P.lexeme lexer
symbol    = P.symbol lexer
parens    = P.parens lexer
semi      = P.semi lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOp= P.reservedOp lexer

texSeqs :: [LaTeX] -> LaTeX
texSeqs = foldl mappend TeXEmpty

--takes care of bug where quotes appear
stripQuotes :: String -> String
stripQuotes str = init (tail str)

showLatex :: LaTeX -> String
showLatex = stripQuotes . show . render

argToLatex :: TeXArg -> LaTeX
argToLatex ta =
    case ta of
      FixArg l -> l
      OptArg l -> l
      MOptArg ls -> texSeqs ls
      SymArg l -> l
      MSymArg ls -> texSeqs ls
      ParArg l -> l
      MParArg ls -> texSeqs ls

argsToLatex :: [TeXArg] -> LaTeX
argsToLatex tas = texSeqs $ map argToLatex tas

{-|
  Given a section of tex, find the string inside \label{...}.
  The map should have the entry ("Labels", [<list of label keywords>]).
  Typically, the label keyword is just "label"
  It will use the first occurrence of label.
-}
findLabel :: [String] -> LaTeX -> String
findLabel labs l =
    case l of
      TeXRaw _ -> ""
      TeXComm str args ->
          if str `elem` labs
             then showLatex $ argsToLatex args
             else findLabel labs (argsToLatex args)
      TeXCommS _ -> ""
      TeXEnv {} -> ""
      TeXMath _ _ -> ""
      TeXLineBreak _ _ -> ""
      TeXBraces latex2 -> findLabel labs latex2
      TeXComment _ -> ""
      TeXSeq latex1 latex2 ->
          let
              ans1 = findLabel labs latex1
              ans2 = findLabel labs latex2
          in
            if ans1 == "" then ans2 else ans1
      TeXEmpty -> ""

{-|
  Given a section of tex, find all strings inside \ref{...}.
  The map should have the entry ("Refs", [<list of label keywords>]).
  Typically, the refs keyword is just "ref".
-}
findRefs :: [String] -> LaTeX -> [String]
findRefs refs l =
    case l of
      TeXRaw _ -> []
      TeXComm str args ->
          if str `elem` refs
             then [showLatex $ argsToLatex args]
             else findRefs refs (argsToLatex args)
      TeXCommS _ -> []
      TeXEnv _ _ latex2 -> findRefs refs latex2
      TeXMath _ latex2 -> findRefs refs latex2
      TeXLineBreak _ _ -> []
      TeXBraces latex2 -> findRefs refs latex2
      TeXComment _ -> []
      TeXSeq latex1 latex2 -> findRefs refs latex1 ++ findRefs refs latex2
      TeXEmpty -> []

{-|
  Given a multimap of Theorems, Proofs, Refs, Labels names, a section of latex, and ProgramInfo, parses the latex into the ProgramInfo.
-}
latexToPI' :: MM.MultiMap String String -> LaTeX -> ProgramInfo -> ProgramInfo
latexToPI' mm latex pi =
    case latex of
      TeXRaw _ -> pi --skip over raw TeX
      TeXComm _ args -> latexToPI' mm (argsToLatex args) pi
      TeXCommS _ -> pi
      TeXEnv str args latex2 -> ifelselist
        [ (str `elem` MM.lookup "Theorems" mm,
           let {
             lab = findLabel (MM.lookup "Labels" mm) latex2;
             name = if not (null args)
                    then
                      case head args of
                        OptArg n -> showLatex n
                    else
                      "" }
           in pi {current = lab} |> insertSF "type" str |> insertField lab |> doIf (name /= "") (insertSF "name" name))
        , (str `elem` MM.lookup "Proofs" mm,
           pi { current = if not (null args)
                          then
                            findLabel (MM.lookup "Refs" mm) (argsToLatex args)
                          else
                            current pi } |> foldIterate insertDep (findRefs (MM.lookup "Refs" mm) latex2))
        ]
        (latexToPI' mm latex2 pi)
      TeXMath _ _ -> pi
      TeXLineBreak _ _ -> pi
      TeXBraces latex2 -> latexToPI' mm latex2 pi
      TeXComment _ -> pi
      TeXSeq latex1 latex2 -> latexToPI' mm latex2 (latexToPI' mm latex1 pi)
      TeXEmpty -> pi

parseLaTeX2 :: String -> LaTeX
parseLaTeX2 str =
    case parseLaTeX $ fromString str of
      Left _ -> TeXEmpty
      Right t -> t

chainPI2:: [String] -> (LaTeX -> ProgramInfo -> ProgramInfo) -> IO ProgramInfo
chainPI2 inputFs parser =
    do
      handles <- mapM (`openFile` ReadMode) inputFs
      contents <- mapM (fmap parseLaTeX2 . hGetContents) handles
      return $ foldl (\ pInfo (fileName, l) -> parser l (pInfo{currentFile = fileName})) emptyPI (zip inputFs contents)

latexAuxParser::ProgramParser
latexAuxParser pi =
  do {eof; return pi}
    <|> try (do {
      string "\\newlabel{";
      lab <- many1 (noneOf "{}");
      many1 (oneOf "{}");
      num <- many1 (noneOf "{}");
      latexAuxParser (insertSF2 lab "num" num pi)})
    <|> do{ many1 (noneOf "\n"); do{eof;return pi} <|> do{string "\n";latexAuxParser pi}}
--figure out how to do pattern matching!

showThm::String -> ProgramInfo -> String
showThm propName pi = removeJustWithDefault (lookupSF propName "type" pi) "" ++ " " ++ removeJustWithDefault ( lookupSF propName "num" pi) "" ++ ": " ++ removeJustWithDefault (lookupSF propName "name" pi) propName

getDepGraph:: ProgramInfo-> Gr String ()
getDepGraph pi =
  let
    mymap = deps pi
    ks = fields pi --MM.keys mymap
    kNums = M.fromList (zip ks [1..])
    --first lookup the key in the map to find dependencies
    --then find the num associated to each key.
    --adjs::Int -> [((), Int)]
    adjs = (\k->
      let
        ds = MM.lookup k mymap
        nums = mapMaybe (`M.lookup` kNums) ds
      in
        fmap (\n -> ((), n)) nums)
  in
    mkGraph (zip [1..] ks) (concatMap (\k -> map (\(x,y) -> (y,lookup2 k kNums,x)) (adjs k)) ks)
    --This has a bug where it won't make edges that reference nodes earlier in the list.
    --buildGr ctxts `debug` (show ctxts)

--should separate out the IO...
--(Gr String ())
--need aux files
latexToDepGraph:: String -> String -> IO ()
latexToDepGraph inputF outputF =
 do
   handle <- openFile inputF ReadMode
   hSetNewlineMode handle universalNewlineMode
   contents <- hGetContents handle
   let myfields = readFields contents
   print myfields
   let inputFs = MM.lookup "Files" myfields
   let auxF = head (MM.lookup "Aux" myfields)
   print auxF
   auxHandle <- openFile auxF ReadMode
   auxContents <- hGetContents auxHandle
   pi <- chainPI2 inputFs (latexToPI' myfields)
   case parse (latexAuxParser pi) "error" auxContents of
     Left err -> print err
     Right pi2 ->
       do
         print pi2
         let graph = getDepGraph pi2
         let dot = defaultDotC2 (\_ l -> showThm l pi2) (\_ l -> lookupSF l "file" pi) graph
         writeFile outputF dot
  --should have safety?

main:: IO ()
main = do
  args <- getArgs
  let inputF = head args
  let outputF = args !! 1
  latexToDepGraph inputF outputF

{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Language.Haskell.Exts.Annotated hiding (layout)
import Language.Haskell.Exts.Extension
import Data.List
import Debug.Trace
import System.IO
import System.Environment( getArgs )
import Data.Generics
import Control.Monad.State

import Layout

data X = X Int Y Y deriving (Data, Typeable)
data Y = Y Int String deriving (Data, Typeable)

anX = X 2 (Y 3 "d") (Y 1 "ee")

layoutGen :: (Data a) => a -> LayoutM () 
layoutGen x = ((everything (>>) $ return () `mkQ` layoutDecl `extQ` layoutExp) x) 

layoutDecl :: Decl SrcSpanInfo -> LayoutM ()
layoutDecl decl = return ()

layoutExp :: Exp SrcSpanInfo -> LayoutM ()
layoutExp e@Do{} = layoutDo e
layoutExp exp    = return ()


-- note maybe make it impossible to access lines and columns from span info, since these cannot be used to compute moves
-- (if token has moved already, pos from span is no longer correct) These errors will be tricky to detect.
layoutDo (Do (SrcSpanInfo doInfo bracketsAndSemisSpans) stmts) =
 do { (doL,doC) <- getLayoutPos $ startPos doInfo
    ; case bracketsAndSemisSpans of
        [] -> return ()
        (doSpan:bracket:semisBracket) ->
         do { -- traceM (concatMap showSpan bracketsAndSemisSpans) -- TODO: find nice combinators to do this stuff 
            ; (obrackLine, _) <- getLayoutPos $ startPos bracket
            ; applyMove (startPos bracket) (doL, doC+3)
            ; applyMove (startPos . ann $ head stmts) (doL, doC + 5)
            ; sequence_ [ do { applyNewlineMove (startPos tk) (doC+3) 
                             ; (semiLine, _) <- getLayoutPos $ startPos tk
                             ; applyMove (startPos $ ann stmt) (semiLine, doC + 5)
                             }
                        | (tk,stmt) <- zip (init semisBracket) (tail stmts) 
                        ]                                    
            ; applyNewlineMove (startPos $ last bracketsAndSemisSpans) (doC+3)
            }
    ; return ()
    }
    
setCol c (l,_) = (l,c)
startPos info = (startLine info, startColumn info)
-- add a way to debug
-- handle options vs language pragma's (and add the one that ghc enables by default) (what about cabal file options??)
-- add comment handling
-- do we need to return only a selection? just the whole do is simpler and makes no difference for Elipse
-- add multiline stuff
-- add do, let, where, data, ..
-- selection may have cursor at start or end. handle this?
-- parse result and compare with src to check if transformation was ok.
-- what are infoPoints? FunBind and Match don't seem to have any


-- Probably not going to handle string gaps (e.g. "bla\     \bla")
-- Range is (Offset, Pos)  0-based
-- Span is (startline, startcol, endline, endcol)  1-based
-- idea:preceding whitespace map:  offset -> preceding whitespace string
-- scan doc, skip {- until matching -}, skip -- until newline, and on non-whitespace put string in map.
-- use this info to move tokens around. (building by hand is tricky, as we need the SrcInfoSpan of the preceding token)    
main :: IO ()
main =
 do { args <- getArgs
    ; case args of
        [offsetStr, lengthStr] ->
         do { let offset = read offsetStr :: Int
                  len = read lengthStr :: Int
            ; doc <- getContents
            ; let (newSelRange, newSelLen, replaceRange, replaceLen, replacementTxt) = layout' doc offset len
            
            ; putStrLn $ show newSelRange ++ " " ++ show newSelLen ++ " " ++
                         show replaceRange ++ " "++ show replaceLen
            ; putStrLn $ replacementTxt
              -- add newline, since Eclipse adds one if last line does not end with it. 
              -- Now we can simply always remove the last character. 
            }
            -- todo: handle incorrect args
    }

layoutTest :: String -> String
layoutTest doc = let (_,_,_,_,doc') = layout' doc 1 1 in doc'

layout' :: String -> Int -> Int -> (Int, Int, Int, Int, String)
layout' doc selOffset selLen =
  let modl = unsafeParse doc
      layout' = execLayout doc $ layoutGen modl
      doc' = showLayout layout'
      (selOffset', selLen') = nudgeRange' doc doc' layout' selOffset selLen 
  in  trace (show layout') $
      (selOffset', selLen', 0, length doc, doc' )
  
layout :: String -> Int -> Int -> (Int, Int, Int, Int, String)
layout doc selRange selLen =
  let ((line,col),_) = rangeToSpan doc selRange selLen 
      modl = unsafeParse doc
  in  case getDeclForSpanModule line col modl of
        Just (FunBind srcInfo ms) -> 
          let declSpan = srcInfoSpan srcInfo
              alignSpanss = map getNamePatternSpansMatch ms
          in  if all (sameLine) alignSpanss then -- only do this if everything before = is on same line as =
                let namePatternRangess = map (map $ spanToRange doc) alignSpanss
                    nips = alignRangess namePatternRangess  -- todo: alignment goes wrong if one match has a guard and one doesn't
                    doc' = applyNips doc nips
                    
                    (declRange, declLen) = spanToRange doc declSpan
                    (declRange',declLen') = nudgeRange nips (declRange, declLen)
                    (selRange',selLen') = nudgeRange nips (selRange, selLen)
                    
                in  -- trace (show nips ++ "\n" ++ doc') $ 
                    (selRange', selLen', declRange, declLen, select (declRange', declLen') doc')
              else (selRange, selLen, 0, 0, "") -- don't do anything
        _      -> (selRange, selLen, 0,0, "")   -- don't do anything

annSp :: Annotated ast => ast SrcSpanInfo -> SrcSpan
annSp = srcInfoSpan . ann

-- start of first and start of last must be on same line
sameLine :: [SrcSpan] -> Bool
sameLine []    = True
sameLine spans = srcSpanStartLine (head spans) == srcSpanStartLine (last spans) 

getSpanHeight :: SrcSpan -> Int
getSpanHeight (SrcSpan _ sl _ el _) = el - sl + 1

data Nip = Nip { nipRange :: Int, nipDisplacement :: Int } deriving Show

-- non-optimized simple implemenation
-- nips are assumed to be sorted
applyNips :: String -> [Nip] -> String
applyNips doc [] = doc
applyNips doc (Nip offset displ : nips) =
  let doc' = applyNips doc nips
  in  if displ < 0 
      then let (left, right) = splitAt (offset + displ) doc'
           in  left ++ drop (-displ) right 
      else let (left, right) = splitAt offset doc'
           in  left ++ replicate displ ' ' ++ right

-- probably not worth optimizing
nudgeRange :: [Nip] -> (Int,Int) -> (Int,Int)
nudgeRange nips (offset, len) = (offsetS, offsetE - offsetS)
 where offsetS = nudgeOffset nips offset
       offsetE = nudgeOffset nips (offset + len) 

nudgeOffset :: [Nip] -> Int -> Int
nudgeOffset [] offset = offset
nudgeOffset (Nip p d : nips) offset =
  let offset' = nudgeOffset nips offset
  in  if d < 0 
      then if p <= offset -- removed sequence is before offset
           then d + offset'        
           else if p + d < offset -- offset is in removed sequence, no recursion, nips are sorted
                then let nudge = offset - (p+d)
                     in  offset - nudge
                else  offset -- removed sequence is after offset, no recursion, nips are sorted
      else if p <= offset then d + offset' else offset -- no recursion, nips are sorted

-- ...xxx...   -> ......
-- ...p        -> ...p
-- ... p       -> ...p
-- ...   p..   -> ...p




nudgeRange' :: String -> String -> Layout -> Int -> Int -> (Int,Int)
nudgeRange' doc doc' layout offset len = posSpanToRange doc' (nudgePos startPos layout) (nudgePos endPos layout)
 where (startPos,endPos) = rangeToSpan doc offset len 

 
alignRangess :: [[(Int, Int)]] -> [Nip]
alignRangess rangeLines =
  let rangeCols = transpose rangeLines
      colWidths  = init $ map (maximum . map snd) rangeCols -- last col is elt after aligned elts
  in  --trace (show colWidths ++ show rangeLines) $ 
      concatMap (alignRanges colWidths) rangeLines 

-- column widths is one shorter than ranges
alignRanges :: [Int] -> [(Int, Int)] -> [Nip]
alignRanges [] [_] = []
alignRanges (w:colWidths) ((o,_):ranges@((o',_):_)) =
  (if o' - o /= w + 1 then [Nip o' $ w - (o' - o) + 1]
                      else [])
  ++ alignRanges colWidths ranges
alignRanges ws os = error $ "alignRanges: incompatible nr of widths and ranges: "++show ws ++ show os 

getNamePatternSpansMatch :: Match SrcSpanInfo -> [SrcSpan]
getNamePatternSpansMatch (Match _ nm pats rh _) = annSp nm : map annSp pats ++ [annSp rh]
getNamePatternSpansMatch (InfixMatch _ pl nm prs rh _) = [annSp pl, annSp nm] ++ map annSp prs ++ [annSp rh]

rangeToSpan :: String -> Int -> Int -> ((Int,Int),(Int,Int))
rangeToSpan doc offset len | offset < 0 = error "rangeToSpan: range offset < 0" -- checks to spot errors in 
                           | len < 0    = error "rangeToSpan: range length < 0" -- other algorithms
                           | otherwise =
  (getPos "range offset" 0 offset lineLengths, getPos "range length" 0 (offset+len) lineLengths)
   where lineLengths = map length $ lines doc
         getPos nm l o []           = error $ nm ++ " too large"
         getPos nm l o (lln : llns) = if o > lln then getPos nm (l+1) (o-lln-1) llns else (l+1, o+1)
-- maybe use scanl


-- ranges are 0-based
-- spans are non-inclusive (i.e. [start, end>)

spanToRange :: String -> SrcSpan -> (Int, Int)
spanToRange doc (SrcSpan _ sl sc el ec) = posSpanToRange doc (sl,sc) (el,ec)


posSpanToRange :: String -> (Int, Int) -> (Int, Int) -> (Int, Int)
posSpanToRange doc (sl, sc) (el, ec) = (offsetS, offsetE - offsetS)
 where offsetS = getOffset sl sc  
       offsetE = getOffset el ec 
       lineLengths = map length $ lines doc
       getOffset l c | l < 1     = error $ "posSpanToRange: line index < 1"
                     | otherwise =
         case splitAt (l-1) lineLengths of 
                (_,         [])     -> error "posSpanToRange: line index too large"
                (preceding, line:_) | c < 1      -> error $ "posSpanToRange: column index < 1" 
                                    | c > line+1 -> error $ "posSpanToRange: column index too large"
                            | otherwise -> sum preceding + l - 1 + c - 1 -- l - 1 is for the newlines

select :: (Int, Int) -> String -> String
select (offset, len) doc = take len $ drop offset doc

prg1 = unlines  
  [ "f x = ()"
  , "m ="
  , " do   putStrLn \"\""
  , "      return   ()"
  , " -- bla"
  , "      return ()"
  , "   "
  , "g :: ()"
  , "g = ()" -- PatBind
  , ""
  , "ff x = ()" -- FunBind
  ]

prg2 = unlines
  [ "f x yyyy z = ()"
  , "f (_:_) _ blaa = ()"
  , "g = ()"
  ]
  
unsafeParse inp = case parseFileContents inp of ParseOk modl -> modl
                                                parseError   -> error $ show parseError

unsafeParseC inp = case parseFileContentsWithComments pm inp of ParseOk modl -> modl
                                                                parseError   -> error $ show parseError

pm = defaultParseMode{extensions=[]}

tst prg = let p = parseFileContents prg :: ParseResult (Module SrcSpanInfo)
      in  case p of --  :: ParseResult (Module SrcSpanInfo) of
            ParseOk modl -> concatMap showSpanInfo $ processModule modl
            r@(ParseFailed _ _) -> show r

showSpanInfo (SrcSpanInfo srcSpan infoPoints) = "Span " ++showSpan srcSpan ++ "[" ++ intercalate ", " (map showSpan infoPoints) ++"]"

showSpan (SrcSpan _ sl sc el ec) = "{"++show sl++":"++show sc++"-"++show el++":"++show ec++"}"

debugSpan str = noInfoSpan $ SrcSpan str 0 0 0 0


showSpans :: (Functor m) => m SrcSpanInfo -> m String
showSpans mod = fmap showShortSpanInfo mod 
 where showShortSpanInfo (SrcSpanInfo srcSpan infoPoints) = showShortSpan srcSpan ++ if null infoPoints then "" else " [" ++ intercalate ", " (map showShortSpan infoPoints) ++"]"
       showShortSpan     (SrcSpan _ sl sc el ec) = show sl++":"++show sc++"-"++show el++":"++show ec
       
isWithinSpan :: Int -> Int -> SrcSpan -> Bool
isWithinSpan line col (SrcSpan _ sl sc el ec) | line < sl || line > el = False 
                                              | line == sl && col >= sc = True
                                              | line > sl && line < el = True
                                              | line == el && col <= ec = True
                                              | otherwise              = False
isWithinSpanInfo :: Int -> Int -> SrcSpanInfo -> Bool
isWithinSpanInfo line col si = isWithinSpan line col $ srcInfoSpan si

getDeclForSpanModule :: Int -> Int -> Module SrcSpanInfo -> Maybe (Decl SrcSpanInfo)
getDeclForSpanModule line col (Module _ _ _  _ decls) = case [d | d<-decls, isWithinSpanInfo  line col $ ann d ] of
                                                          [d] -> Just d
                                                          []  -> Nothing
                                                          _   -> error "Multiple declarations" 
getDeclForSpanModule line col _                       = Nothing

getDeclsModule :: Module SrcSpanInfo -> [Decl SrcSpanInfo]
getDeclsModule (Module _ _ _  _ decls) = decls
getDeclsModule _                       = []


processModule :: Module SrcSpanInfo -> [SrcSpanInfo]
processModule (Module _ _ _  _ decls) = concatMap processDecl decls
processModule _                       = []

processDecl (PatBind _ _ _ rhs _) = processRhs rhs
processDecl (FunBind _ matches)   = concatMap processMatch matches
processDecl _                     = []


processMatch (Match _ _ _ rhs _)        = processRhs rhs
processMatch (InfixMatch _ _ _ _ rhs _) = processRhs rhs

processRhs (UnGuardedRhs _ exp) = processExp exp
processRhs (GuardedRhss _ guardedrhss) = concatMap processGuardedRhs guardedrhss

processGuardedRhs (GuardedRhs _ _ exp) = processExp exp

processExp (Do si stmts)  = map processStmt stmts
processExp _             = []

processStmt stmt = ann stmt

traceM :: Monad m => String -> m ()
traceM str = trace str $ return ()

show1Decl TypeDecl{}         = "TypeDecl{}"       
show1Decl TypeFamDecl{}      = "TypeFamDecl{}"    
show1Decl DataDecl{}         = "DataDecl{}"       
show1Decl GDataDecl{}        = "GDataDecl{}"      
show1Decl DataFamDecl{}      = "DataFamDecl{}"    
show1Decl TypeInsDecl{}      = "TypeInsDecl{}"    
show1Decl DataInsDecl{}      = "DataInsDecl{}"    
show1Decl GDataInsDecl{}     = "GDataInsDecl{}"   
show1Decl ClassDecl{}        = "ClassDecl{}"      
show1Decl InstDecl{}         = "InstDecl{}"       
show1Decl DerivDecl{}        = "DerivDecl{}"      
show1Decl InfixDecl{}        = "InfixDecl{}"      
show1Decl DefaultDecl{}      = "DefaultDecl{}"    
show1Decl SpliceDecl{}       = "SpliceDecl{}"     
show1Decl TypeSig{}          = "TypeSig{}"        
show1Decl FunBind{}          = "FunBind{}"        
show1Decl PatBind{}          = "PatBind{}"        
show1Decl ForImp{}           = "ForImp{}"         
show1Decl ForExp{}           = "ForExp{}"         
show1Decl RulePragmaDecl{}   = "RulePragmaDecl{}" 
show1Decl DeprPragmaDecl{}   = "DeprPragmaDecl{}" 
show1Decl WarnPragmaDecl{}   = "WarnPragmaDecl{}" 
show1Decl InlineSig{}        = "InlineSig{}"      
show1Decl InlineConlikeSig{} = "InlineConlikeSig{}"
show1Decl SpecSig{}          = "SpecSig{}"        
show1Decl SpecInlineSig{}    = "SpecInlineSig{}"  
show1Decl InstSig{}          = "InstSig{}"        
show1Decl AnnPragma{}        = "AnnPragma{}"

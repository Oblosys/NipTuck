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
import qualified Data.Map as Map -- to be removed when we have correct primitives in Layout
import Data.Map (Map)
import Control.Monad

import Layout


-- todo 
-- Check for tabs!
-- check/fix block & line comments
-- currently line comments may be formatted so tokens appear after them
-- solution: take commentStart and comment together and on setting layout, check preceding token for --
-- (or we can add an extra field to the Layout type)
-- at least make eclipse show output instead of "Could not load".
-- also show stderror in error notification
-- format do keeps adding newlines
t2 x = case x of
     Just _ -> "a"
     Nothing -> "b"
    ++ "7"
test = do { putStrLn "" ; return ()}

formatGen :: (Data a) => a -> LayoutM () 
formatGen x = ((everything (>>) $ return () `mkQ` formatDecl `extQ` formatExp) x) 

formatDecl :: Decl SrcSpanInfo -> LayoutM ()
formatDecl d@DataDecl{} = formatDataDecl d
--formatDecl d@FunBind{}  = formatFunBind d
formatDecl decl         = return ()

formatExp :: Exp SrcSpanInfo -> LayoutM ()
formatExp e@Do{}   = formatDo e
formatExp e@Case{} = formatCase e
formatExp exp      = return ()



--DataDecl l (DataOrNew l) (Maybe (Context l)) (DeclHead l) [QualConDecl l] (Maybe (Deriving l))  
whenJust :: Monad m => Maybe x -> (x -> m ()) -> m()
whenJust mx f = maybe (return ()) f mx

-- todo: maybe check if data decl is single line, and if so, don't align |'s with =
formatDataDecl (DataDecl (SrcSpanInfo _ (eq:ors)) _ mContext declHead conDecls mDeriving) =
 do { whenJust  mContext $ \context ->
        applyLayout context 0 1
    ; applyLayout declHead 0 1
    
    ; applyLayout eq 0 1
    ; (_,eqC) <- getLayoutPos eq
    ; sequence_ [ applyLayout or 1 (eqC - 1) | or <- ors] -- todo applyIndent which subtracts the 1
    ; sequence_ [ applyLayout conDecl 0 1 | conDecl <- conDecls]
    ; whenJust  mDeriving $ \der ->
       do { applyLayout der 0 1
          ; case der of
               Deriving _ (ih:_) -> applyLayout ih 0 1
               _                 -> return ()
          }
    }


-- because formatter is applied top down, it works with nested do's (since the column is taken from the do keyword)

-- todo: handle multiline statements, and modify indentation in a smart way
-- note: maybe make it impossible to access lines and columns from span info, since these cannot be used to compute moves
-- (if token has moved already, pos from span is no longer correct) These errors will be tricky to detect.
formatDo (Do (SrcSpanInfo doInfo bracketsAndSemisSpans) stmts) =
 do { (doL,doC) <- getLayoutPos $ startPos doInfo
    ; case bracketsAndSemisSpans of
        [] -> return ()
        (doSpan:bracket:semisBracket) ->
         do { -- traceM (concatMap showSpan bracketsAndSemisSpans) -- TODO: find nice combinators to do this stuff 
            ; applyLayout bracket 0 1
            ; let indent = doC+3 - 1 -- to indent to column c, we need c-1 spaces
            ; applyLayout (head stmts) 0 1
            ; sequence_ [ do { applyLayout tk 1 indent 
                             ; applyLayout stmt 0 1
                             }
                        | (tk,stmt) <- zip (init semisBracket) (tail stmts) 
                        ]                                    
            ; applyLayout (last bracketsAndSemisSpans) 1 indent
            }
    ; return ()
    }

-- todo: handle multiline statements, and modify indentation in a smart way (more important than for do, because
-- we don't use brackets) 
-- todo: take guards into account
formatCase (Case (SrcSpanInfo _ (case_:of_:_)) exp alts) =
 do { applyLayout exp 0 1
    ; applyLayout of_ 0 1
    ; (_,caseC) <- getLayoutPos case_
    
    ; let patArrowSpans = [(annPos alt, annPos galts) | alt@(Alt _ _ galts _) <- alts ]
    ; patWidths <-  sequence [ getWidth tk nextTk | (tk,nextTk) <- patArrowSpans ]
    ; let maxWidth = maximum patWidths
    ; let fills = map (maxWidth-) patWidths
    ; sequence_ [ do { applyLayout pat 1 (caseC + 2 - 1)
                     ; applyLayout arrow 0 (fill+1)
                     } | (fill,(pat,arrow)) <- zip fills patArrowSpans]
    
    ; sequence_ [ case galts of
                    UnGuardedAlt _ exp -> applyLayout (annPos exp) 0 1
                    _                  -> return ()
                | alt@(Alt _ _ galts _) <- alts ] 
    }


-- todo: take guards into account
formatFunBind (FunBind _ ms) = 
 do { let alignSpanss = map getNamePatternSpansMatch ms
    --; traceM $ show alignSpanss -- name arg1 .. argn rhs
    ; sameLines <- sequence [ sameLine l r | (l,_,r) <- alignSpanss ]
    ; when (and sameLines) $ 
 do { widthss <- sequence [ sequence [ getWidth tk nextTk | (tk,nextTk) <- zip ms (tail $ ms ++ [r]) ]
                          | (l, ms, r) <- alignSpanss
                          ] 
    --; traceM $ show widthss
    ; let colWidths = map maximum $ transpose widthss
    --; traceM $ show colWidths
    ; let fillss = zipWith (zipWith (-)) (repeat colWidths) widthss -- nr of spaces needed to be as wide as widest arg in colum
    --; traceM $ show fillss
    ; sequence_ [ do { applyLayout arg 0 1
                     ; sequence_ [ applyLayout tk 0 (fill + 1) | (tk,fill) <- zip (args++[rh]) fills] 
                     } 
                | ((_,arg:args,rh), fills )<- zip alignSpanss fillss]
    ; return ()
    }}
    
-- todo name, these are poss, not spans
getNamePatternSpansMatch :: Match SrcSpanInfo -> (Pos, [Pos], Pos)
getNamePatternSpansMatch (Match _ nm pats rh _)        = (annPos nm, map annPos pats, annPos rh)
getNamePatternSpansMatch (InfixMatch _ pl nm prs rh _) = (annPos pl, annPos nm : map annPos prs , annPos rh)

-- return true if p2 starts on the same line as p1
sameLine :: Pos -> Pos -> LayoutM Bool
sameLine p1 p2 = do { (l1,_) <- getLayoutPos p1
                    ; (l2,_) <- getLayoutPos p2
                    ; return $ l1 == l2
                    }
                    
-- precondition p1 and p2 are on the same line 
getWidth :: Pos -> Pos -> LayoutM Int
getWidth p1 p2 = do { (_,c1) <- getLayoutPos p1
                    ; (_,c2) <- getLayoutPos p2
                    ; layout <- get
                    ; case Map.lookup p2 layout of
                        Just (_,ss,_) -> return $ c2 - ss - c1
                        Nothing       -> error $ "getWidth: lookup on non-existent token: "++show p2
                    }
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
            ; let (newSelRange, newSelLen, replaceRange, replaceLen, replacementTxt) = formatEnclosingDecl doc offset len
            
            ; putStrLn $ show newSelRange ++ " " ++ show newSelLen ++ " " ++
                         show replaceRange ++ " "++ show replaceLen
            ; putStrLn $ replacementTxt
              -- add newline, since Eclipse adds one if last line does not end with it. 
              -- Now we can simply always remove the last character. 
            }
            -- todo: handle incorrect args
    }

formatTest :: String -> IO ()
formatTest doc = let (_,_,_,_,doc') = formatEnclosingDecl doc 1 1 in putStrLn doc'

-- note that due to the way nudge works, no layout can be added to the front or the back of the formatted selection
-- (e.g. ">f x = 1<" -> "\n\n   >f x = 1<   ")
-- (back is tricky anyway, as we keep preceding whitespace. 
formatEnclosingDecl :: String -> Int -> Int -> (Int, Int, Int, Int, String)
formatEnclosingDecl doc selOffset selLen =
  let modl = unsafeParse doc -- todo: handle error here
      ((lin,col),_) = rangeToSpan doc selOffset selLen
  in  case getDeclForSpanModule lin col modl of
        Nothing -> (selOffset, selLen, 0, 0, "") -- not in a declaration, don't do anything
        Just decl ->
          let (declOffset, declLen) = spanToRange doc . srcInfoSpan $ ann decl
              formattedLayout = execLayout doc $ formatGen decl
              doc' = showLayout formattedLayout
              (declOffset', declLen') = nudgeRange' doc doc' formattedLayout declOffset declLen
              (selOffset', selLen') = nudgeRange' doc doc' formattedLayout selOffset selLen 
          in  --trace (show (declOffset, declLen) ++ "\n" ++ show doc ++ show doc' ++ "\n" ++ show formattedLayout) $
              (selOffset', selLen', declOffset, declLen, select (declOffset', declLen') doc' )
              -- For EclipseFP it is better to only replace the change decl, instead of entire source,
              -- since undo will select the replaced part for some reason.
  
layoutOld :: String -> Int -> Int -> (Int, Int, Int, Int, String)
layoutOld doc selRange selLen =
  let ((line,col),_) = rangeToSpan doc selRange selLen 
      modl = unsafeParse doc
  in  case getDeclForSpanModule line col modl of
        Just (FunBind srcInfo ms) -> 
          let declSpan = srcInfoSpan srcInfo
              alignSpanss = map getNamePatternSpansMatch' ms
          in  if all sameLine' alignSpanss then -- only do this if everything before = is on same line as =
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

annPos :: Annotated ast => ast SrcSpanInfo -> Pos
annPos = startPos . srcInfoSpan . ann

-- start of first and start of last must be on same line
sameLine' :: [SrcSpan] -> Bool
sameLine' []    = True
sameLine' spans = srcSpanStartLine (head spans) == srcSpanStartLine (last spans) 

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

getNamePatternSpansMatch' :: Match SrcSpanInfo -> [SrcSpan]
getNamePatternSpansMatch' (Match _ nm pats rh _) = annSp nm : map annSp pats ++ [annSp rh]
getNamePatternSpansMatch' (InfixMatch _ pl nm prs rh _) = [annSp pl, annSp nm] ++ map annSp prs ++ [annSp rh]


startPos info = (startLine info, startColumn info)

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

-- todo bug: handle (l,c) (l',0), where l' may be (nr of lines+1)
posSpanToRange :: String -> (Int, Int) -> (Int, Int) -> (Int, Int)
posSpanToRange doc (sl, sc) (el, ec) = -- trace (show (doc, sl,sc,el,ec)) $ 
                                       (offsetS, offsetE - offsetS)
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

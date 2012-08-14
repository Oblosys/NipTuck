{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances #-}
module Layout where

import Language.Haskell.Lexer hiding (Pos)
import Control.Monad.State
import qualified Data.Map as Map
import Data.Map (Map)
import Debug.Trace
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Annotated.Syntax

type Newlines = Int
type Spaces = Int
type Pos = (Int,Int)



-- TODO: layout model trims lines with trailing whitespace. Do we need to extend the model?
-- todo: this also holds for whitespace at end of file. 
type Layout = Map Pos (Newlines, Spaces, String) -- first Pos identifies the token
-- invariant: layout is sorted

type LayoutM = State Layout

execLayout :: String -> LayoutM () -> Layout
execLayout src m = execState m (initLayout src) 


class TokenPos x where
  tokenPos :: x -> Pos

instance TokenPos Pos where
  tokenPos = id
  
-- explicit instances (instead of using SrcInfo) to get better error messages
instance TokenPos SrcLoc where
  tokenPos x = (srcLine x, srcColumn x)

-- todo: do we really need FlexibleInstances & UndecidableInstances for these instances?
instance TokenPos SrcSpan where
  tokenPos x = (srcSpanStartLine x, srcSpanStartColumn x)

instance TokenPos SrcSpanInfo where
  tokenPos x = tokenPos (srcInfoSpan x)

instance Annotated ast => TokenPos (ast SrcSpanInfo) where
  tokenPos x = tokenPos $ ann x
  
-- check if preceding tk is line comment if newline == 0
applyLayout :: TokenPos p => p -> Newlines -> Spaces -> LayoutM ()
applyLayout tgt newlines spaces = modify $ Map.adjust (\(_,_,tkStr) -> (newlines, spaces, tkStr)) (tokenPos tgt)  

getLayoutPos :: TokenPos p => p -> LayoutM Pos
getLayoutPos tgt =
 do { layout <- get
    ; return $ getLayoutPos' (tokenPos tgt) 1 1 $ Map.toList layout  
    }

-- todo: name, and make function f = getLayoutPos' tgt 1 1
getLayoutPos' tgt ln cl [] = error $ "getLayout on non-existent token"++show tgt
getLayoutPos' tgt ln cl ((pos,(ns,ss,tkStr)):tokens) =
  let ln' = ln+ns
      cl' = ss + if ns == 0 then cl else 1
  in  if  pos == tgt 
      then (ln',cl')
      else getLayoutPos' tgt ln' (cl'+length tkStr) tokens
  
-- todo: what are Layout and Indent? can they appear in the token stream?
initLayout :: String -> Layout
initLayout src = Map.fromList . processTokens 1 1 . filter ((Whitespace /=) . fst) $ lexerPass0 src
 where processTokens _      _      []                   = []
       processTokens prevLn prevCl ((tkType,(pos,tkStrRaw)):tokens) =
         let (tkLn,tkCl) = (line pos, column pos)
             (newlines, spaces) | tkLn == prevLn = (0, tkCl-prevCl)
                                | otherwise      = (tkLn-prevLn, tkCl-1)
             tkStr = case tkType of Comment -> case reverse tkStrRaw of ('\n':_) -> init tkStrRaw
                                                                        _        -> tkStrRaw
                                    _       -> tkStrRaw
         in case tkType of 
              NestedComment -> let commentLine:commentLines = lines tkStrRaw -- string is not empty
                               in  ((tkLn, tkCl), (newlines, spaces, commentLine)) :
                                   [ ((l,1),(1,0,comml)) | (l,comml) <- zip [tkLn+1..] commentLines]
              _             -> [((tkLn, tkCl), (newlines, spaces, tkStr)) ] 
            ++ processTokens tkLn (tkCl + length tkStr) tokens
  
--  [ ((line pos, column pos), (line pos, column pos, tokenStr)) | (_,(pos,tokenStr)) <- lexerPass0 src ]

showLayout :: Layout -> String
showLayout layout = concatMap showToken $ Map.elems layout
 where showToken (newlines, spaces, tokenStr) = replicate newlines '\n' ++ replicate spaces ' ' ++ tokenStr 
{-
showLayout layout = showLayout' ""  1 1 layout
 where showLayout' revStr _  _  []                            = reverse revStr
       showLayout' revStr ln cl ((_,(tln, tcl, tstr)):tokens) =
         let (ln', cl') = moveLinesCols tln tcl $ tstr
         in  showLayout' (reverse tstr ++ applyLayout tln tcl ln cl revStr) ln' cl' tokens  

applyLayout targetLn targetCol currentLn currentCol revStr =
  if targetLn < currentLn 
  then dropUntil targetLn targetCol currentLn currentCol revStr
  else if currentLn == targetLn 
       then if targetCol < currentCol
            then dropUntil targetLn targetCol currentLn currentCol revStr
            else replicate (targetCol - currentCol) ' ' ++ revStr
       else  replicate (targetCol - 1) ' ' ++ replicate (targetLn - currentLn) '\n' ++ revStr

-- todo: fix so we can add spaces even when dropping (e.g. moving to 1,3 on "a\n" -> "a ")
-- todo: no need to traverse all spaces until we're at the right line.    
dropUntil targetLn targetCol currentLn currentCol revStr | currentLn == targetLn && currentCol == targetCol = revStr
                                                         | otherwise =
  case revStr of
    [] -> error "moving beyond start"
    (' ':  revStr') -> dropUntil targetLn targetCol currentLn (currentCol-1) revStr' 
    ('\n': revStr') -> dropUntil targetLn targetCol (currentLn-1) (1 + (length $ takeWhile (/='\n') revStr')) revStr'
    _               -> error "Dropping non-whitespace characters"

-}

{-

layoutMove :: Pos -> Pos -> Layout -> Layout
layoutMove tgt _ [] =  error $ "Moving token that is not in layout "++show tgt
layoutMove tgt newPos@(newLn, newCl) layout@(l@(orgPos,(tln, tcl, tstr)):ls) | orgPos /= tgt = l : layoutMove tgt newPos ls 
                                                                             | otherwise     =
  let deltaL = newLn - tln
      deltaC = newCl - tcl
  in  tweakLines deltaL $ tweakColsOnLine tln deltaC layout   
      
      
tweakColsOnLine _        _      []                             = []
tweakColsOnLine thisLine deltaC layout@((orgPos,(tln, tcl, tstr)):ls) | tln == thisLine = (orgPos,(tln,tcl+deltaC,tstr)) :
                                                                                          tweakColsOnLine thisLine deltaC ls
                                                                      | otherwise       = layout

tweakLines _      []                             = []                                                                
tweakLines deltaL ((orgPos,(tln, tcl, tstr)):ls) = (orgPos,(tln+deltaL, tcl, tstr)) : tweakLines deltaL ls
-}
-- todo: this doesn't take positions in whitespace into account (check what happens when whitespace is removed)
-- maybe get rid of whitespace tokens altogether?
--todo: fix nudge so   stat|;  leads to stat|\n  ; instead of stat\n  |;    (| is cursor)  
nudgePos :: Pos -> Layout -> Pos
nudgePos pos layout = nudgePos' layout pos $ Map.toList layout

nudgePos' layout (line,col) [] = (-1, -1)
nudgePos' layout (line,col) (((orgLn, orgCol),(ns,ss,tkStr)):tokens) =
  if (line,col) > (orgLn,orgCol+length tkStr) 
  then nudgePos' layout (line,col) tokens 
  else -- either on the token (including immediately past it), or in its whitespace
       let (nl,nc) = getLayoutPos' (orgLn, orgCol) 1 1 $ Map.toList layout
       in  if (line,col) >= (orgLn,orgCol) -- on the token -- maybe rewrite to line == orgLn && ..
           then (nl,nc+col-orgCol)
           else if line == orgLn -- in the whitespace but on the same line
                then --trace (show $ ((orgCol-col), ss) )
                     (nl, nc - ((orgCol-col) `min` ss)) 
                else --trace ("orgLn - ln = "++show (orgLn - line)++", ns="++show ns) $
                     if orgLn - line < ns 
                     then (nl - (orgLn -line), 1) -- in the new line part of the whitespace
                     else -- before the first newline, so next to previous token
                          case break ((==(orgLn,orgCol)).fst) $ Map.toList layout of -- todo ugly
                            ([], _) -> (1,1) -- no preceding token
                            (precTks, _) -> let (precedingTokenPos, (_,_,precTkStr)) = last precTks
                                                (precTkLn, precTkCl) = getLayoutPos' precedingTokenPos 1 1 $ Map.toList layout
                                            in  (precTkLn, precTkCl+length precTkStr)
   --1 "f"
   --2 ""
   --3 ""
   --4 ""
   --5 "     ="
   --   123456
   --1 "f"
   --2 ""
   --3 "  ="
   -- "ff     =   222"
   --  123456789
   -- "ff  =   222"
   -- 3 -> 3 4 -> 4 5 -> 4  8 -> 5
nudgePos'' :: Pos -> [(Pos, (Newlines,Spaces,String))] -> Pos
nudgePos'' _          []                    = error "nudgePos: empty layout"
nudgePos'' (line,col) layout@(firstToken:_) =
  case break (\((l,c),_) -> (l,c) > (line,col)) layout of
    (tks@(_:_), nextTokens) -> 
      let ((l,c),(ns,ss,tstr)) = last tks
          (nl,nc) = getLayoutPos' (l,c) 1 1 layout
          bound = case nextTokens of
                    --(_,(rl,rc,_)):_ -> (rl,rc) -- make sure not to extend beyond next token
                    _              -> (nl,nc+length tstr)   -- or past the end of the current one if there is no next
                    
      in -- trace (show (l,c,nl,nc,line-l, col-c)) 
            (line-l+nl,col-c+nc) `min` bound   
    _   -> error $ "nudgePos: position " ++ show (line,col) ++ " is before first token: " ++ show firstToken
-- todo: add case for missing token to the right


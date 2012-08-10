module Layout where

import Language.Haskell.Lexer hiding (Pos)
import Control.Monad.State

type Line = Int   -- 1-based
type Column = Int -- 1-based
type Pos = (Line, Column)


type Layout = [(Pos, (Line, Column, String))] -- first line x column identifies token, second line x column is position in the layout
-- invariant: layout is sorted

type LayoutM = State Layout

execLayout :: String -> LayoutM () -> Layout
execLayout src m = execState m (initLayout src) 

applyMove :: Pos -> Pos -> LayoutM ()
applyMove tgt newPos = modify $ layoutMove tgt newPos 
     
applyNewlineMove :: Pos -> Column -> LayoutM ()
applyNewlineMove tgt newCol = 
 do { (oldLine,_) <- getLayoutPos tgt
    ; modify $ layoutMove tgt (oldLine+1,newCol) 
    }

initLayout :: String -> Layout
initLayout src = [ ((line pos, column pos), (line pos, column pos, tokenStr)) | (_,(pos,tokenStr)) <- lexerPass0 src ]

getLayoutPos :: Pos -> LayoutM Pos
getLayoutPos tgt =
 do { layout <- get
    ; case lookup tgt layout of
        Nothing -> error $ "getLayout on non-existent token"++show tgt
        Just (ln,cl, _) -> return (ln,cl)
    }
showLayout :: Layout -> String
showLayout layout = showLayout' ""  1 1 layout
 where showLayout' revStr _  _  []                            = reverse revStr
       showLayout' revStr ln cl ((_,(tln, tcl, tstr)):tokens) =
         let (ln', cl') = moveLinesCols tln tcl $ tstr
         in  showLayout' (reverse tstr ++ applyLayout tln tcl ln cl revStr) ln' cl' tokens  

moveLinesCols ln cl []     = (ln, cl)
moveLinesCols ln cl (c:cs) = let (ln',cl') = case c of 
                                               '\n' -> (ln+1, 1)
                                               _    -> (ln, cl+1) -- don't take tabs into account
                             in  moveLinesCols ln' cl' cs

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

-- todo: this doesn't take positions in whitespace into account (check what happens when whitespace is removed)
-- maybe get rid of whitespace tokens altogether?
--todo: fix nudge so   stat|;  leads to stat|\n  ; instead of stat\n  |;    (| is cursor)  
nudgePos :: Pos -> Layout -> Pos
nudgePos (line,col) layout =
  case break (\((l,c),(nl,nc,_)) -> (l,c) > (line,col)) layout of
    (tks@(_:_), (_,(rl,rc,_)):_) -> let ((l,c),(nl,nc,tstr)) = last tks
                      in -- trace (show (l,c,nl,nc,line-l, col-c)) 
                         (line-l+nl,col-c+nc) `min` (rl,rc)   --- make sure not to extend beyond next token
    _              -> error "TODO: make a nice error"
-- todo: add case for missing token to the right
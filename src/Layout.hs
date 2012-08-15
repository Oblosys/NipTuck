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
type Line = Int
type Column = Int
type Pos = (Line,Column)


-- TODO: layout model trims lines with trailing whitespace. Do we need to extend the model?
-- todo: this also holds for whitespace at end of file. 
type Layout = Map Pos (Newlines, Spaces, String) -- first Pos identifies the token
-- invariant: layout is sorted

type LayoutList = [(Pos, (Newlines, Spaces, String))]

type LayoutM = State Layout

execLayout :: String -> LayoutM () -> Layout
execLayout src m = execState m (initLayout src) 

traceM :: Monad m => String -> m ()
traceM str = trace str $ return ()

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

-- todo: switch args 
-- check if preceding tk is line comment if newline == 0
applyLayout :: TokenPos p => p -> Newlines -> Spaces -> LayoutM ()
applyLayout tgt newlines spaces = modify $ Map.adjust (\(_,_,tkStr) -> (newlines, spaces, tkStr)) (tokenPos tgt)  

-- Put tk on a new line and indent to column col. Preserve any newlines in front of tk.
applyNewlineIndent :: TokenPos p => Column -> p -> LayoutM ()
applyNewlineIndent col tk =
 do { (nwlns, _) <- getTokenLayout tk  
    ; applyLayout tk (nwlns `max` 1) (col-1) -- to end up in column col, we need to add col - 1 spaces
    }

getLayoutPos :: TokenPos p => p -> LayoutM Pos
getLayoutPos tgt =
 do { layout <- get
    ; return $ getLayoutPos' (tokenPos tgt) 1 1 $ Map.toList layout  
    }
    
getTokenLayout :: TokenPos p => p -> LayoutM (Newlines,Spaces)
getTokenLayout tgt =
 do { layout <- get
    ; return $ case Map.lookup (tokenPos tgt) layout of
                 Nothing -> error $ "getTokenLayout on non-existent token"++show (tokenPos tgt)
                 Just (newlines,spaces,_) -> (newlines,spaces)
    }
 
-- modify layout for target and adjust following lines so they remain at the same column relative to
-- the target's column. We need to pass the reference column, because tgt may already have moved
-- before applying this layout
-- todo: maybe use referencePos instead of column because it is easier. This will allow confusion with tgt though. 
-- todo: maybe we only want to do this if the next line is at least at target's column. (this is a sign
-- that the layout is not related to the target)
applyLayoutAndReindent referenceCl tgt lastTk newlines spaces =
 do { --traceM $ "reindent: "++show (tokenPos tgt) ++ " " ++ show lastTk ++ " " ++ show newlines ++ " " ++ show spaces
    ; applyLayout tgt newlines spaces
    ; (_, newCl) <- getLayoutPos tgt
    ; let (tgtLn,_) = tokenPos tgt
    ; tokensPossToIndent <- getTokensPossBetween (tgtLn+1,0) (tokenPos lastTk) -- NB we select the tokens based on original positions
    ; --traceM $ "shifting "++show (newCl-referenceCl)++": " ++ show tokensPossToIndent
    ; shiftLines (newCl-referenceCl) tokensPossToIndent
    }
    
shiftLines _     [] = return ()
shiftLines shift ((orgPos,(currentLn, currentCl)):tokens) =
 do { shiftColumn shift orgPos
    ; shiftLines' currentLn tokens
    }
 where shiftLines' _    []                                       = return ()
       shiftLines' line ((orgPos,(currentLn, currentCl)):tokens) =
         if currentLn == line 
         then shiftLines' line tokens
         else do { shiftColumn shift orgPos
                 ; shiftLines' currentLn tokens
                 }
                 
shiftColumn shift tgt = do { (newlines, spaces) <- getTokenLayout tgt
                           ; applyLayout tgt newlines ((spaces + shift) `max` 1)
                           }

-- todo: name, and make function f = getLayoutPos' tgt 1 1
getLayoutPos' tgt ln cl layout = case lookup tgt $ getLayoutPoss ln cl layout of
                             Nothing -> error $ "getLayoutPos' on non-existent token"++show tgt
                             Just pos -> pos
  
-- compute current positions for all tokens
getLayoutPoss :: Int -> Int -> LayoutList -> [(Pos,Pos)]
getLayoutPoss _ _ [] = []
getLayoutPoss ln cl ((pos,(ns,ss,tkStr)):tokens) =
  let ln' = ln+ns
      cl' = ss + if ns == 0 then cl else 1
  in  (pos, (ln',cl')) : getLayoutPoss ln' (cl'+length tkStr) tokens
  
  

-- includes end
getTokensPossBetween :: Pos -> Pos -> LayoutM [(Pos,Pos)]
getTokensPossBetween start end = do { layout <- get
                                    ; let tokenPoss = getLayoutPoss 1 1 $ Map.toList layout
                                    ; return $ filter (\(p,_) -> (start <= p && p <= end)) $ tokenPoss
                                    }


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

-- todo: this doesn't take positions in whitespace into account (check what happens when whitespace is removed)
-- maybe get rid of whitespace tokens altogether?
--todo: fix nudge so   stat|;  leads to stat|\n  ; instead of stat\n  |;    (| is cursor)  
nudgePos :: Pos -> Layout -> Pos
nudgePos pos layout = --trace ("nudgePos "++show pos ++ " (" ++ show layout ++ ")") $ 
                      nudgePos' layout pos $ Map.toList layout

nudgePos' layout (line,col) [] = error "nudgePos' on empty list"
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

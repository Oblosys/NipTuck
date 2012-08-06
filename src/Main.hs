module Main where

import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Extension
import Data.List
import Debug.Trace
import System.IO
import System.Environment( getArgs )

main :: IO ()
main =
 do { args <- getArgs
    ; case args of
        [offsetStr, lengthStr] ->
         do { let offset = read offsetStr :: Int
                  len = read lengthStr :: Int
            ; inp <- getContents
            ; putStrLn $ show offset ++ " 1 "++show offset ++ " "++show len
            ; putStrLn $ "{-"++take len (drop offset inp)++"-}"
              -- add newline, since Eclipse adds one if last line does not end with it. 
              -- Now we can simply always remove the last character. 
            }
            -- todo: handle incorrect args
    }

offsetToSpan :: String -> Int -> Int -> ((Int,Int),(Int,Int))
offsetToSpan doc offset len = (getPos 0 offset lineLengths, getPos 0 (offset+len) lineLengths)
 where lineLengths = map length $ lines doc
       getPos l o []           = (l, o) -- error
       getPos l o (lln : llns) = if o > lln then getPos (l+1) (o-lln-1) llns else (l, o)
        
prg = unlines  
  [ "f ="
  , " do   putStrLn \"\""
  , "      return   ()"
  , " -- bla"
  , "      return ()"
  , "   "
  , "g :: ()"
  , "g = ()"
  ]

m = case parseFileContents prg of ParseOk modl -> modl

pm = defaultParseMode{extensions=[]}

tst = let p = parseFileContents prg :: ParseResult (Module SrcSpanInfo)
      in  case p of --  :: ParseResult (Module SrcSpanInfo) of
            ParseOk modl -> concatMap showSpanInfo $ processModule modl
            r@(ParseFailed _ _) -> show r

showSpanInfo (SrcSpanInfo srcSpan infoPoints) = "Span " ++showSpan srcSpan ++ "[" ++ intercalate ", " (map showSpan infoPoints) ++"]"

showSpan (SrcSpan _ sl sc el ec) = "{"++show sl++":"++show sc++"-"++show el++":"++show ec++"}"

debugSpan str = noInfoSpan $ SrcSpan str 0 0 0 0

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

processExp (Do si stmts)  = trace (showSpanInfo si) $ map processStmt stmts
processExp _             = []

processStmt stmt = ann stmt

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

{-
hlayout selection
hlayout file --line= --column=

returns layed out function (or entire file for some --entire flag)
as well as new cursor/selection 

.hlayout file or pass flags

Eclipse:
command that calls external executable with selection or entire file + cursor/selection
and replaces selection with layout 




-}

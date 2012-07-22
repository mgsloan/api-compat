{-# LANGUAGE TupleSections #-}

module ApiCompat.GhcApi where

import Control.Applicative                 ( (<$>) )
import Control.Arrow                       ( (***) )
import Control.Monad.Trans                 ( liftIO )
import Data.Either                         ( either )
import Data.Char                           ( isAlphaNum, toLower, isSpace )
import Data.List                           ( intersperse )
import Language.Haskell.Interpreter        ( ModuleElem(..), getModuleExports, loadModules, interpret, reset, setTopLevelModules )
import Language.Haskell.Interpreter.Unsafe ( unsafeRunInterpreterWithArgs )
-- import System.Console.GetOpt
import System.Directory                    ( removeFile )
import System.FilePath                     ( takeFileName, dropExtension )
import System.IO                           ( openTempFile, hClose, hPutStr )

getApis pkgs mods
  = fail_left =<< unsafeRunInterpreterWithArgs (map ("-package "++) pkgs) exec
 where
  exec = do
    splices <- mapM mk_splice mods

    (path, handle) <- liftIO $ openTempFile "." "GetApi.hs"


    let mod_name = filter isAlphaNum . takeFileName $ dropExtension path

        source = unlines
          $ [ "{-# LANGUAGE TemplateHaskell #-}"
            , "module " ++ mod_name ++ " where"
            , "import ApiCompat.ShowInfo ( showInfo )"
            , "import Prelude ( putStr, unlines, concat, ($), Char )"
            ]
         ++ map ("import " ++) mods
         ++ [ "", "main = putStr output" ]
         ++ [ "", "output = unlines $ concat", "  [" ]
         ++ zipWith (++) ("    " : repeat "  , ") splices
         ++ [ "  ]" ]

    liftIO $ do
      hPutStr handle source
      hClose handle
  
    loadModules [path]
    setTopLevelModules [mod_name]
    result <- interpret (mod_name ++ ".output") ""
    reset
    liftIO $ removeFile path
    return result

  fail_left = either (fail . show) return
  mk_splice m = (("$(showInfo \"" ++ m ++ "\" [") ++) . (++ "] )")
              . concat . intersperse ", " . map process
            <$> getModuleExports m
   where
    prefixed p n 
      | head n == '(' = p ++ "(" ++ m ++ "." ++ (init $ tail n) ++ ")"
      | otherwise     = p ++ m ++ "." ++ n
    process (Fun   n)   = prefixed "'"  n
    process (Class n _) = prefixed "''" n
    process (Data  n _) = prefixed "''" n
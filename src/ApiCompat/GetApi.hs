{-# LANGUAGE TupleSections #-}

module ApiCompat.GetApi
  ( getApi ) where

import Control.Applicative                 ( (<$>) )
import Control.Monad.Trans                 ( liftIO )
import Control.Exception                   ( finally )
import Data.Char                           ( isAlphaNum )
import Data.List                           ( intersperse )
import Language.Haskell.Interpreter        ( ModuleElem(..), getModuleExports, loadModules, interpret, reset, setTopLevelModules )
import Language.Haskell.Interpreter.Unsafe ( unsafeRunInterpreterWithArgs )
-- import System.Console.GetOpt
import System.Directory                    ( removeFile )
import System.FilePath                     ( takeFileName, dropExtension )
import System.IO                           ( openTempFile, hClose, hPutStr )
import System.Process                      ( readProcess )

type PackageName = String
type ModuleName = String

getApi :: [PackageName] -> [ModuleName] -> IO String
getApi pkgs mods = do
  (path, handle) <- openTempFile "." "GetApi.hs"

  let args = map ("-package=" ++) pkgs

  results <- (`finally` removeFile path) .
    unsafeRunInterpreterWithArgs args $ do
      splices <- mapM mk_splice mods

      let mod_name = filter isAlphaNum . takeFileName $ dropExtension path

          source = unlines
            $ [ "{-# LANGUAGE TemplateHaskell #-}"
              , "module " ++ mod_name ++ " where"
              , "import ApiCompat.ShowInfo ( showInfos )"
              , "import Prelude ( putStr, unlines, concat, ($), Char )"
              ]
           ++ map ("import " ++) mods
           ++ [ "", "main = putStr output" ]
           ++ [ "", "output = $(showInfos", "  [" ]
           ++ zipWith (++) ("    " : repeat "  , ") splices
           ++ [ "  ] )" ]

      liftIO $ do
        hPutStr handle source
        hClose handle

      liftIO $ readProcess "runhaskell" (args ++ [path]) ""

  either (fail . show) return results
 where
  mk_splice m = (("(\"" ++ m ++ "\", [") ++) . (++ "] )")
              . concat . intersperse ", " . map process
            <$> getModuleExports m
   where
    prefixed p n 
      | head n == '(' = p ++ "(" ++ m ++ "." ++ (init $ tail n) ++ ")"
      | otherwise     = p ++ m ++ "." ++ n
    process (Fun   n)   = prefixed "'"  n
    process (Class n _) = prefixed "''" n
    process (Data  n _) = prefixed "''" n

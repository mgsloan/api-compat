{-# LANGUAGE TupleSections #-}

module ApiCompat.GetApi
  ( getApi ) where

import Control.Applicative                 ( (<$>) )
import Control.Exception                   ( finally )
import Data.Char                           ( isAlphaNum )
import Data.List                           ( intersperse )
import Language.Haskell.Interpreter        ( ModuleElem(..), getModuleExports )
import Language.Haskell.Interpreter.Unsafe ( unsafeRunInterpreterWithArgs )
import System.Directory                    ( removeFile )
import System.FilePath                     ( takeFileName, dropExtension )
import System.IO                           ( openTempFile, hClose, hPutStr )
import System.Process                      ( readProcess )

--TODO: instead use "InteractiveEval.getInfo"

--TODO: use Distribution.InstalledPackageInfo to collect modules, versions, etc.
-- (see ghc/utils/ghc-pkg/Main.hs describePackage)

type PackageName = String
type ModuleName = String

getApi :: [PackageName] -> [ModuleName] -> IO String
getApi pkgs mods = do
  (path, handle) <- openTempFile "." "GetApi.hs"

  (`finally` removeFile path) $ do
    let args = map ("-package=" ++) pkgs

    splices <- either (fail . show) return
           =<< unsafeRunInterpreterWithArgs args
             ( mapM mk_splice mods )

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

    hPutStr handle source
    hClose handle

    readProcess "runhaskell" (args ++ [path]) ""
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

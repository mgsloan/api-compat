{-# LANGUAGE PatternGuards, ParallelListComp, TemplateHaskell #-}
module ApiCompat.ShowInfo where

import Control.Applicative           ( (<$>) )
import Data.Char                     ( isAlphaNum )
import Data.Function                 ( on )
import Data.Generics                 ( Data, Typeable, listify, everywhere )
import Data.Generics.Aliases         ( extT )
import Data.List                     ( sort, groupBy, intersperse )
import qualified Data.Map as M
import Language.Haskell.TH
import Language.Haskell.TH.Lift      ( lift )
import Language.Haskell.TH.Instances ( )
import Language.Haskell.TH.PprLib
import Language.Haskell.TH.Syntax    (Name(..), NameFlavour(..))

--TODO: remove contexts from methods
--TODO: list instances next to classes / datatypes

showInfos :: [(String, [Name])] -> ExpQ
showInfos ms = lift . unlines . concat =<< mapM show_info ms
 where
  show_info (m, ns) = do
    infos <- mapM safe_reify ns
    return
      $ [ "----------------------------------------"
        , "module " ++ m
        ] ++ map (either id (("\n"++) . show . infoDoc)) (sort infos)
          ++ [""]
  safe_reify n = recover (return . Left $ "-- Couldn't reify " ++ pprint n)
                         (Right <$> reify n)

infoDoc :: Info -> Doc
infoDoc info = case info of
  (ClassI d _)                 -> dec $ deUnique d
--  (ClassOpI)                 ->
  (TyConI d)                   -> dec $ deUnique d
  (FamilyI d _)                -> dec $ deUnique d
--  (PrimTyConI n i b)         ->
--  (DataConI )                ->
  (VarI n t _ f)               -> vcat [fixity f n, dec . SigD n $ deUnique t]
--  (TyVarI n t)               ->
  _                            -> empty
 where
  tyvars = hcat . punctuate space . map ppr

  commaize = hcat . punctuate (text ", ")

  vnest l = (l $+$) . nest 2 . vcat

  name n | all (not . isAlphaNum) $ nameBase n = parens $ ppr n
         | otherwise = ppr n

  prd (ClassP n ts) = hcat $ punctuate space (name n : map flat_typ ts)
  prd (EqualP l r) = flat_typ l <+> text "~" <+> flat_typ r

  prds [] = text "()"
  prds xs = parens . foldl1 ($$)
          $ [nl] ++ (intersperse (text ",") . map prd $ sort xs) ++ [nl]
   where nl = text ""

  context = (<+> text "=>") . prds

  derivings = parens . commaize . map ppr . sort

  forall tvs ctx = hcat [ text "forall ", tyvars tvs, text ". " ]
               $+$ context ctx

  flat_typ (ForallT v c t) = parens $ hcat [forall v c, flat_typ t]
  flat_typ (SigT t k)      = parens $ hcat [flat_typ t, text "::", ppr k]
  flat_typ (VarT n)        = name n
  flat_typ (ConT n)        = name n
  flat_typ ArrowT          = text "(->)"
  flat_typ ListT           = text "[]"
  flat_typ (AppT ListT r)  = brackets $ flat_typ r
  flat_typ t@(AppT (AppT ArrowT _) _)
    = parens . hcat $ punctuate (text " -> ") $ map flat_typ ts
--    = hcat [flat_typ l, text " ->", flat_typ r]
   where
    ts = tyUnArrow t
  flat_typ (AppT ArrowT l) = parens $ hcat [flat_typ l, text " ->"]
  flat_typ t = case f of
    (TupleT        i) -> parens (tup_txt i)
    (UnboxedTupleT i) -> parens $ hcat [text "# ", tup_txt i, text " #"]
    _                 -> hcat $ punctuate space flats
   where
    (f:rs) = tyUnApp t
    flats = map flat_typ rs
    tup_txt i = commaize flats <> text (replicate (i - length flats) ',')

  typ (ForallT tvs ctx t) 
    =   hcat [text "forall ", tyvars tvs, text "."]
    $+$ nest 2 (context ctx $+$ typ t)

  typ t = vb_infix flat_typ "->" $ tyUnArrow t

  vb_infix f s xs
    = vcat $ map ((<+> text s) . f) (init xs) ++ [f $ last xs]

  vf_infix _ _ [] = empty
  vf_infix f s (x:xs)
    = vcat $ text (replicate (length s) ' ') <+> f x
           : map ((text s <+>) . f) xs

  dec (ClassD ctx n tvs fds decs) = vnest (text "class" <+> context ctx)
    [ vnest (hcat [name n, space, tyvars tvs, pp_fds, text " where"])
            (map dec decs)
    ]
   where
    pp_fds | [] <- fds = empty
           | otherwise = text "|" <+> commaize (map ppr fds)

  dec (SigD n t) = name n <+> text "::" $$ nest 2 (typ t)

  dec (DataD    ctx n tvs cs ds) = datalike "data "    ctx n tvs  cs  ds

  dec (NewtypeD ctx n tvs cs ds) = datalike "newtype " ctx n tvs [cs] ds

--  dec (TySynD n tvs t) = hcat 

  dec _ = empty

  strict s = text $ case s of
    NotStrict -> ""
    IsStrict  -> "!"
    Unpacked -> "{-# UNPACK #-} !"

  field (NotStrict, t) = flat_typ t
  field (s,         t) = strict s <+> parens (flat_typ t)

  rec_field (n, s, t) = name n <+> text "::" <+> field (s, t)

  con (NormalC n fs)      = vnest (name n) $ map field fs
  con (RecC    n fs)      = vnest (name n) $ map rec_field fs
  con (InfixC l n r)      = vnest (name n) $ map field [l, r]
  con (ForallC tvs ctx c) = vnest (forall tvs ctx) [con c]

  datalike d ctx n tvs cs ds = vnest (hcat [text d, context ctx])
    [ hcat [name n, space, tyvars tvs, enull $ text " ="]
    , enull $ vf_infix con "|" cs
    , text "deriving" <+> derivings ds
    ]
   where
    enull x = if null cs then empty else x

  fixity (Fixity l d) n
    = text $ concat [ "infix", dir, " ", show l, " ", show $ name n ]
   where
    dir = case d of
      InfixL -> "l"
      InfixR -> "r"
      InfixN -> ""

tyUnApp :: Type -> [Type]
tyUnApp = reverse . helper
 where
  helper (AppT l r) = r : helper l
  helper x          = [x]

tyUnArrow :: Type -> [Type]
tyUnArrow = helper
 where
  helper (AppT (AppT ArrowT l) r) = l : helper r
  helper x                        = [x]

everywhereMaybe :: (Typeable a, Data b) => (a -> Maybe a) -> b -> b
everywhereMaybe f = everywhere (id `extT` replace)
 where
  replace x | Just x' <- f x = x'
            | otherwise      = x

deUnique :: Data a => a -> a
deUnique d = everywhereMaybe (`M.lookup` rewrites) d
 where
  rewrites
    = M.fromList . concatMap (process . map deUniqueName)
    . groupBy ((==) `on` nameBase) $ allNames d

  process [n] = [n]
  process ns 
    = [ (n, mkName $ nameBase n' ++ replicate p '\'')
      | (n, n') <- ns
      | p <- [0..]
      ]

  deUniqueName n@(Name o (NameU _)) = (n, Name o NameS)
  deUniqueName n                    = (n, n)

allNames :: Data a => a -> [Name]
allNames = map head . groupBy (==) . sort
         . listify (const True :: Name -> Bool)
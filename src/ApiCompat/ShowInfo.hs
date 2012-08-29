{-# LANGUAGE PatternGuards, ParallelListComp, TemplateHaskell, CPP #-}
module ApiCompat.ShowInfo where

import Control.Applicative           ( (<$>) )
import Data.Char                     ( isAlphaNum )
import Data.Function                 ( on )
import Data.Generics                 ( Data, Typeable, listify, everywhere )
import Data.Generics.Aliases         ( extT )
import Data.Maybe                    ( fromJust, listToMaybe )
import Data.Ord                      ( comparing )
import Data.List                     ( sort, sortBy, groupBy, intersperse )
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
        ] ++ map (either id (("\n"++) . show . infoDoc)) (sortBy comp infos)
          ++ [""]
  safe_reify n = recover (return . Left $ "-- Couldn't reify " ++ pprint n)
                         (Right <$> reify n)
  comp (Left  _) _         = LT
  comp (Right _) (Left  _) = GT
  comp (Right l) (Right r) = comparing infoName l r

infoDoc :: Info -> Doc
infoDoc info = case info of
  (ClassI d _)       -> dec False $ deUnique d
--(ClassOpI)         ->
  (TyConI d)         -> dec False $ deUnique d
  (FamilyI d _)      -> dec False $ deUnique d
--(PrimTyConI n i b) ->
--(DataConI )        ->
  (VarI n t _ f)     -> vcat [fixity f n, dec False . SigD n $ deUnique t]
--(TyVarI n t)       ->
  _                  -> empty
 where
  tyvars = hcat . punctuate space . map ppr

  commaize = hcat . punctuate (text ", ")

  vnest l = (l $+$) . nest 2 . vcat

  name n | all (not . isAlphaNum) $ nameBase n = parens $ ppr n
         | otherwise = ppr n

  prd (ClassP n ts) = hcat $ punctuate space (name n : map flat_typ ts)
  prd (EqualP l r) = flat_typ l <+> text "~" <+> flat_typ r

  prds [] = text "()"
  prds xs = parens . nest 1 . foldl1 ($$)
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
    _                 -> hcat $ punctuate space (flat_typ f : flats)
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

  dec _ (ClassD ctx n tvs fds decs) = vnest (text "class" <+> context ctx)
    [ vnest (hcat [name n, space, tyvars tvs, pp_fds, text " where"])
            (map (dec True) decs)
    ]
   where
    pp_fds | [] <- fds = empty
           | otherwise = text "|" <+> commaize (map ppr fds)

  dec _ (SigD         n     t    ) = name n <+> text "::" $$ nest 2 (typ t)

  dec _ (DataD    ctx n tvs cs ds) = datalike "data "    ctx n tvs  cs  ds

  dec _ (NewtypeD ctx n tvs cs ds) = datalike "newtype " ctx n tvs [cs] ds

  dec _ (TySynD       n tvs t    ) = text "type" <+> name n <+> tyvars tvs
                                 <+> text "=" <+> flat_typ t

  dec inClass (FamilyD flavour n tvs k)
    = text header <+> name n <+> tyvars tvs $$ kind
   where
    header
      = (if inClass then id else (++" family"))
      $ case flavour of
          TypeFam -> "type"
          DataFam -> "data"
    kind = maybe empty (nest 2 . (text "::" <+>) . ppr) k

  dec _ x = text $ "-- can't pretty-print:\n" ++ show x

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

-- Utils

tyRemoveForall :: Type -> Type
tyRemoveForall (ForallT _ _ t) = t
tyRemoveForall              t  = t

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

decName :: Dec -> Maybe Name
decName (FunD         n _            )  = Just n
decName (TySynD       n _ _          )  = Just n
decName (FamilyD      _ n _ _        )  = Just n
decName (NewtypeD     _ n _ _ _      )  = Just n
decName (DataD        _ n _ _ _      )  = Just n
decName (ClassD       _ n _ _ _      )  = Just n
decName (InstanceD    _ t _          )  | ((ConT n):_) <- tyUnApp t = Just n
                                        | otherwise                 = Nothing
decName (DataInstD    _ n _ _ _      )  = Just n
decName (NewtypeInstD _ n _ _ _      )  = Just n
decName (ValD         p _ _          )  = listToMaybe $ patNames p
decName (SigD         n _            )  = Just n
decName (TySynInstD   n _ _          )  = Just n
decName (ForeignD (ImportF _ _ _ n _))  = Just n
decName (ForeignD (ExportF _   _ n _))  = Just n
#if __GLASGOW_HASKELL__ < 706
decName (PragmaD (InlineP n _      ))   = Just n
decName (PragmaD (SpecialiseP n _ _))   = Just n
#else
decName (PragmaD (InlineP     n _ _ _)) = Just n
decName (PragmaD (SpecialiseP n _ _ _)) = Just n
#endif
decName _                               = Nothing

patNames :: Pat -> [Name]
patNames (VarP        n     ) = [n]
patNames (TupP        p     ) = concatMap patNames p
patNames (UnboxedTupP p     ) = concatMap patNames p
patNames (InfixP      l _ r ) = patNames l ++ patNames r
patNames (UInfixP     l _ r ) = patNames l ++ patNames r
patNames (ParensP     p     ) = patNames p
patNames (BangP       p     ) = patNames p
patNames (TildeP      p     ) = patNames p
patNames (AsP         n p   ) = n : patNames p
patNames (RecP        _ f   ) = concatMap (patNames . snd) f
patNames (ListP       p     ) = concatMap patNames p
patNames (SigP        p _   ) = patNames p
patNames (ViewP       _ p   ) = patNames p
patNames _                    = []

infoName :: Info -> Name
infoName (ClassI     d _    ) = fromJust $ decName d
infoName (ClassOpI   n _ _ _) = n
infoName (TyConI     d      ) = fromJust $ decName d
infoName (FamilyI    d _    ) = fromJust $ decName d
infoName (PrimTyConI n _ _  ) = n
infoName (DataConI   n _ _ _) = n
infoName (VarI       n _ _ _) = n
infoName (TyVarI     n _    ) = n

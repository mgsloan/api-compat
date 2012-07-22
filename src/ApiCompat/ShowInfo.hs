module ApiCompat.ShowInfo where

import Control.Applicative                 ( (<$>) )
import Data.List                           ( sort )
import Language.Haskell.TH
import Language.Haskell.TH.Lift            ( lift )
import Language.Haskell.TH.Instances
import Language.Haskell.TH.Ppr
import Language.Haskell.TH.PprLib


showInfo :: String -> [Name] -> ExpQ
showInfo m ns = do
  infos <- mapM safe_reify ns
  lift $ [ "----------------------------------------"
         , "module " ++ m ]
      ++ map (either id (("\n"++) . show . display)) (sort infos)
      ++ [""]
  
 where
  safe_reify n = recover (return . Left $ "-- Couldn't reify " ++ pprint n)
                         (Right <$> reify n)

display info = case info of
  (ClassI d _)                 -> d_dec d
--  display (ClassOpI)         ->
  (TyConI d)                   -> d_dec d
  (FamilyI d _)                -> d_dec d
--  display (PrimTyConI n i b) ->
--  display (DataConI )        ->
  (VarI n t _ f)               -> vcat [d_fixity f n, d_dec $ SigD n t]
--  display (TyVarI n t)       ->
  _                            -> empty
 where
  d_tvs = hcat . punctuate space . map ppr

  commaize = hcat . punctuate (text ", ")

  d_name n = parens $ ppr n

  d_ctx = parens . commaize . map d_pred . sort

  d_deriving = parens . commaize . map ppr . sort

  v_infix f s xs = vcat $ map ((<+> text s) . f) (init xs) ++ [f $ last xs]

  d_pred (ClassP n ts) = hcat $ punctuate space (d_name n : map d_flat_typ ts)
  d_pred (EqualP l r) = d_flat_typ l <+> text " ~ " <+> d_flat_typ r

  d_forall (ForallT tvs ctx t) = hcat
    [ text "forall ", d_tvs tvs, text ". ", d_ctx ctx, text " => " ]

  d_flat_typ ft@(ForallT _ _ t) = parens $ hcat [d_forall ft, d_flat_typ t]
  d_flat_typ (SigT t k)         = parens $ hcat [d_flat_typ t, text "::", ppr k]
  d_flat_typ (VarT n)           = d_name n
  d_flat_typ (ConT n)           = d_name n
  d_flat_typ ArrowT             = text "(->)"
  d_flat_typ ListT              = text "[]"
  d_flat_typ (AppT ListT r)     = brackets $ d_flat_typ r
  d_flat_typ (AppT (AppT ArrowT l) r)
    = parens . hcat $ punctuate (text " -> ") $ map d_flat_typ ts
--    = hcat [d_flat_typ l, text " ->", d_flat_typ r]
   where
    ts = tyUnArrow r
  d_flat_typ (AppT ArrowT l) = parens $ hcat [d_flat_typ l, text " ->"]
  d_flat_typ t = case f of
    (TupleT        i) -> parens (tup_txt i)
    (UnboxedTupleT i) -> parens $ hcat [text "# ", tup_txt i, text " #"]
    t                 -> d_flat_typ t
   where
    (f:rs) = tyUnApp t
    flats = map d_flat_typ rs
    tup_txt i = hcat [commaize flats, text $ replicate (i - length flats) ',']

  d_typ (ForallT tvs ctx t) = vcat
    [ hcat
        [ text "forall ", d_tvs tvs, text ". ", d_ctx ctx, text " =>"
        ]
    , nest 2 $ d_typ t
    ]

  d_typ t = v_infix d_flat_typ "->" $ tyUnArrow t

  d_dec (ClassD ctx n tvs fds decs) = vcat
    [ text "class" <+> d_ctx ctx
    , hcat [text " => ", d_name n, space, d_tvs tvs, pp_fds, text " where"]
    , nest 2 . vcat $ map d_dec decs
    ]
   where
    pp_fds | [] <- fds = empty
           | otherwise = text "|" <+> commaize (map ppr fds)

  d_dec (SigD n t) = d_name n <+> text "::" $$ nest 2 (d_typ t)

  d_dec (DataD    ctx n tvs cs ds) = d_data "data "    ctx n tvs  cs  ds

  d_dec (NewtypeD ctx n tvs cs ds) = d_data "newtype " ctx n tvs [cs] ds

--  d_dec (TySynD n tvs t) = hcat 

  d_dec _ = empty

  d_con = ppr

  d_data d ctx n tvs cs ds = vcat
    [ text d, d_ctx ctx
    , hcat [text " => ", d_name n, space, d_tvs tvs, enull $ text " ="]
    , enull $ v_infix d_con "|" cs
    , text "deriving " <+> d_deriving ds
    ]
   where
    enull x = if null cs then empty else x

  d_fixity (Fixity l d) n
    = text $ concat [ "infix", dir, " ", show l, " ", show $ d_name n ]
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

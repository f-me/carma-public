{-# LANGUAGE LambdaCase #-}

{-
  An QuasiQuoter for postresql-simple that helps extracts data from model
  that kinda more safe and robust than deal with plain text SQL and only
  values interpolation.

  There's three types of interpolation blocks:
    1. $(T|…)$ - table name from model where '…' is module name of a model
    2. $(F|…)$ - field name from model
       TODO fields from parent models
    3. $(V|…)$ - a value

  An usage example:
    [msql|
      SELECT $(F|Usermeta.group)$
           , $( V | 100 + 500 )$::int
      FROM $( T
            | Usermeta
            )$"
    |]
  You could see that here is demonstrated that spaces or line-breaks
  can be used inside interpolation blocks for decorative aligning
  (but usually you don't need it).

  Keep in mind that $(T|…)$ counts on that any model always have `ident` field.
  Also model must be imported with `as` alias, like this (or with `qualified`):
    import Carma.Model.ModelName as ModelName
  so you can rich `ModelName.ident`. But if you don't mind you could write it
  this way:
    $(T|Carma.Model.ModelName)$
  So this means this is actually module of a model name, not exactly model name.
-}

{-
  It's recommened to import everything exported here implicitly because any of
  these is required to deal with result produced by the quoter.

  But `parseQuery` isn't required, so you could "hide" it if you don't need it.

  `parseQuery` is supposed to be used when you need this quoter logic without
  quasi-quoters. For instance you could implement kinda ORM DSL and produce
  String that will be parsed using `parseQuery` later.
-}
module Data.Model.Utils.PostgreSQL.MSqlQQ
     ( msql
     , parseQuery

     , Only (..)
     , (:.) (..)
     , plainTableName
     , plainFieldName
     ) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote (QuasiQuoter (..))
import           Language.Haskell.Meta (parseExp)

import           Database.PostgreSQL.Simple (Only (..), (:.) (..))
import           Data.List (dropWhileEnd)

import           Data.Model.Utils.PostgreSQL.InterpolationHelpers
                   (plainFieldName, plainTableName)


data SQL
   = PlainSQL String
   | Interpolation InterpolationBlock
   deriving (Eq, Show)

data InterpolationBlock
   = T String -- Table name
   | F String -- Field name
   | V String -- Value
   deriving (Eq, Show)


{-
  "OverloadedStrings" extension is recommended to use with to interpret result
  SQL as a ByteString that required by postgresql-simple's requests.
-}
msql :: QuasiQuoter
msql = QuasiQuoter
  { quotePat  = error "Cannot use `msql` as a pattern"
  , quoteType = error "Cannot use `msql` as a type"
  , quoteDec  = error "Cannot use `msql` as a declaration"
  , quoteExp  = parseQuery
  }


{-
  Tranforms string like:
    "select $(F|Usermeta.group)$, $(V|100 + 500)$::int from $(T|Usermeta)$"
  To template that will produce query like this:
    ( "select ?, ? from ?"
    , (  ()
      :. Only (plainFieldName Usermeta.group)
      :. Only (100 + 500)
      :. Only (plainTableName Usermeta.ident)
      )
    )
-}
parseQuery :: String -> ExpQ
parseQuery = buildExp . reverse . map revPlain . f []
  where
  f :: [SQL] -> String -> [SQL]
  f sql "" = sql

  -- Interpolation block is opened, parsing it…
  f sql ('$':'(':xs) =
    let (block, rest) = parseInterpolation xs
     in f (Interpolation block : sql) rest

  -- Create new plain sql block when
  f [] (x:xs) = f [PlainSQL $ x : ""] xs

  -- Append to plain sql block or add new one
  f sql (x:xs) =
    case head sql of
         PlainSQL s -> f (PlainSQL (x : s)  : tail sql) xs
         _          -> f (PlainSQL (x : "") : sql     ) xs

  revPlain :: SQL -> SQL
  revPlain (PlainSQL x) = PlainSQL $ reverse x
  revPlain x            = x


{-
  Fetching interpolation block until it closes with ")$" and parses its innards.
-}
parseInterpolation :: String -> (InterpolationBlock, String)
parseInterpolation = f ""
  where
  f :: String -> String -> (InterpolationBlock, String)
  f _ ""               = error "Interpolation block is not closed"
  f block (')':'$':xs) = (innards $ reverse block, xs)
  f block (x:xs)       = f (x : block) xs

  -- Parsing innards between "$(" and ")$"
  innards :: String -> InterpolationBlock
  innards s = case splitType "" s of
    ("",  _) -> error "Type of interpolation block is empty"
    (_,  "") -> error "Interpolation block is empty"
    ("T", x) -> T $ map onlySpaces x
    ("F", x) -> F $ map onlySpaces x
    ("V", x) -> V $ map onlySpaces x
    (t,   _) -> error $ "Unknown interpolation type: " ++ show t

  spaces = "\t\r\n " :: String
  trim = dropWhileEnd (`elem` spaces) . dropWhile (`elem` spaces)

  -- This example:
  --   $( V | 100
  --        + 500 )$
  -- will be interpreted `V "100      + 500"`
  onlySpaces x | x `elem` spaces = ' ' | otherwise = x

  -- Parse and split type of interpolation and its content
  splitType :: String -> String -> (String, String)
  splitType _ ""       = error "Incorrect interpolation syntax"
  splitType t ('|':xs) = (trim $ reverse t, trim xs)
  splitType t (x:xs)   = splitType (x:t) xs


{-
  Builds final template by parsed blocks.
-}
buildExp :: [SQL] -> ExpQ
buildExp s = tupE [stringE queryStr, buildInterpolations interpolations]
  where
  queryStr = mconcat $ map (\case PlainSQL x -> x ; Interpolation _ -> "?") s

  interpolations :: [ExpQ]
  interpolations =
    map interpolationToExp $
    foldr (\x acc -> case x of Interpolation y -> y : acc ; _ -> acc) [] s

  interpolationToExp = appE (conE $ mkName "Only") . \case
    T x -> appE (varE $ mkName "plainTableName") (varE $ mkName $ x ++ ".ident")
    F x -> appE (varE $ mkName "plainFieldName") (varE $ mkName x)
    V x -> pure $ case parseExp x of Right y -> y ; Left e -> error e

  buildInterpolations :: [ExpQ] -> ExpQ
  buildInterpolations []     = tupE []
  buildInterpolations (x:xs) = tupE [] `cons` foldl cons x xs
    where cons = flip uInfixE (conE $ mkName ":.")

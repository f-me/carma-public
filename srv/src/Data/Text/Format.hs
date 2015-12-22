{-# LANGUAGE DefaultSignatures, FlexibleInstances #-}

-- | Format string with named args
--
-- >format "My name is $name, I am $age years old" ["name" %= "Joe", "age" %= 24]
-- >-- "My name is Joe, I am 24 years old"
--
-- To escape '$', double it. Format string can be also used recursively with @(%%)@ instead of @(%=)@
--
-- >format "$$x is $x, and $$r is $r" ["x" %= 5, "r" %% "$x + $x"] -- Note (%%) instead of (%=)
-- >-- "$x is 5, and $r is 5 + 5"
module Data.Text.Format (
    FormatValue(..),
    FormatArg, FormatArgs,
    FormatShow(..),
    format,
    (%=), (%%)
    ) where

import Data.Char (isAlpha)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)

data FormatValue = FormatString String | FormatPure Text

type FormatArg = (String, FormatValue)
type FormatArgs = [FormatArg]

-- | format function. Accepts format string and arguments:
-- >format "My name is $name, I am $age years old" ["name" %= "Joe", "age" %= 24]
-- >-- "My name is Joe, I am 24 years old"
format :: String -> FormatArgs -> Text
format fmt args = pack $ format' fmt args where
    format' :: String -> FormatArgs -> String
    format' [] args = []
    format' ('$':('$':fmt)) args = '$' : format' fmt args
    format' ('$':fmt) args = case name of
        "" -> '$' : format' fmt args
        _ -> case fromMaybe (error $ "Format argument '" ++ name ++ "' expected") (lookup name args) of
            FormatString f -> format' (f ++ fmt') args
            FormatPure s -> unpack s ++ format' fmt' args
        where
        (name, fmt') = span isAlpha fmt
    format' (c:fmt) args = c : format' fmt args

-- | FormatShow class, by default using @show@
class FormatShow a where
    formatShow :: a -> Text
    default formatShow :: Show a => a -> Text
    formatShow = pack . show

instance FormatShow String where
    formatShow = pack

instance FormatShow Char where
    formatShow = pack . return

instance FormatShow Int
instance FormatShow Integer
instance FormatShow Double
instance FormatShow Float
instance FormatShow Bool

instance FormatShow Text where
    formatShow = id

infixr 1 %=
infixr 1 %%

-- | Used to form argument
(%=) :: FormatShow a => String -> a -> FormatArg
name %= value = (name, FormatPure $ formatShow value)

-- | Form argument as format string
(%%) :: String -> String -> FormatArg
name %% fmt = (name, FormatString fmt)

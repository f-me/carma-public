-- For russian language features
-- See also: http://nashol.com/2011060955536/tablica-padejei-russkogo-yazika.html
module Utils.ShowCase
     ( class ShowCase
     , showNominative
     , showGenitive
     , showDative
     , showAccusative
     , showAblative
     , showPrepositive
     ) where


class ShowCase a where
  showNominative  :: a -> String
  showGenitive    :: a -> String
  showDative      :: a -> String
  showAccusative  :: a -> String
  showAblative    :: a -> String
  showPrepositive :: a -> String

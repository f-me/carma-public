{-# LANGUAGE ExistentialQuantification #-}

{-|

Text interpreter for 'Backoffice' DSL terms.

-}

module Carma.Backoffice.Text
    (
      backofficeText
      -- * Lookup tables for Ident DSL constants
    , IBox(..)
    , IMap
    , lkp
    )

where

import           Prelude hiding ((>), (==), (||), (&&), const)
import qualified Prelude as P ((==))

import           Control.Applicative
import           Control.Monad.Trans.Reader

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock
import           Data.Typeable

import           Data.Model

import           Carma.Backoffice.DSL


-- | Text interpreter context.
data TCtx = TCtx { identMap :: IMap
                 }


-- | Text embedding for Backoffice DSL types.
newtype TextE t = TextE { toText :: Reader TCtx Text }


-- | Simple TextE constructor which does not use the context.
textE :: Text -> TextE t
textE t = TextE (return t)


-- | Format binary function arguments.
textBinary :: TextE a
           -> TextE b
           -> Text
           -- ^ Opening bracket.
           -> Text
           -- ^ Argument separator.
           -> Text
           -- ^ Closing bracket.
           -> Reader TCtx Text
textBinary a b open between close = do
  a' <- toText a
  b' <- toText b
  return $ T.concat [open, a', between, b', close]


-- | TextE constructor for trigger terms.
triggerText :: TextE a
            -> TextE v
            -> TextE b
            -> Reader TCtx Text
triggerText field value body = do
  trig' <- textBinary field value "Когда " " приобретает значение " ""
  body' <- toText body
  return $ T.concat $ [trig', ": "] ++ [body']


-- | Existential container for model idents.
--
-- Used to store idents for multiple models in a single lookup table
-- ('IMap'). Eq and Ord instances use both numeric value and model
-- 'TypeRep'.
data IBox = forall m. Model m => IBox (IdentI m)


instance Show IBox where
    show (IBox i) = show i


instance Eq IBox where
    (IBox b1@(Ident i1)) == (IBox b2@(Ident i2)) =
        (typeOf b1, i1) P.== (typeOf b2, i2)


instance Ord IBox where
    compare (IBox b1@(Ident i1)) (IBox b2@(Ident i2)) =
        (typeOf b1, i1) `compare` (typeOf b2, i2)


type IMap = Map IBox Text


-- | Convert an ident to text.
lkp :: IBox -> IMap -> Text
lkp k@(IBox k'@(Ident i)) env =
    maybe
    (T.pack $ show k')
    (\t -> T.concat [t, "#", T.pack $ show i])
    (Map.lookup k env)


instance Backoffice TextE where
    now = textE "Текущее время"
    since dt t =
        TextE $ (\c -> T.concat [c, " + ", formatDiff dt]) <$> (toText t)
    before dt t =
        TextE $ (\c -> T.concat [c, " - ", formatDiff dt]) <$> (toText t)

    role r =
        TextE $ T.append "Пользователи с ролью " <$> (toText $ const r)
    currentUserOr r =
        TextE $
        T.append "Текущий пользователь и другие с ролью " <$> (toText $ const r)

    previousAction = textE "Предыдущее действие"

    userField     = textE . fieldDesc
    caseField     = textE . fieldDesc
    serviceField  = textE . fieldDesc

    onField f v body = TextE $ triggerText (textE $ fieldDesc f) v body

    not v = TextE $
            T.append "НЕ выполнено условие " <$> toText v

    a > b = TextE $ textBinary a b "" " > " ""

    a == b = TextE $ textBinary a b "" " равно " ""
    a && b = TextE $ textBinary a b "(" ") и (" ")"
    a || b = TextE $ textBinary a b "(" ") или (" ")"

    const v = TextE $ lkp (IBox v) . identMap <$> ask

    just v = TextE $ lkp (IBox v) . identMap <$> ask

    req v = TextE $ flip T.snoc '*' <$> toText v

    oneOf val set =
        TextE $ do
          val' <- toText val
          set' <- mapM (toText . const) set
          return $ T.concat [val', " ∈ {", T.intercalate ", " set', "}"]

    switch conds ow =
        TextE $ do
          let ppc (cond, act) =
                  textBinary cond act "Если " ", то " ""
          ow' <- toText ow
          conds' <- mapM ppc conds
          return $ T.concat [ T.intercalate "; " conds'
                            , "; во всех других случаях — "
                            , ow'
                            ]

    setServiceField acc i =
        TextE $
        (\c -> T.concat [fieldDesc acc, " ← ", c]) <$> toText i

    sendSMS i =
        TextE $
        T.append "Отправить SMS по шаблону " <$> toText (const i)

    sendPSAMail = textE "Отправить письмо в PSA"

    sendDealerMail = textE "Отправить письмо дилеру"

    sendGenserMail = textE "Отправить письмо в Genser"

    closeWith acts r =
        TextE $ do
          acts' <- mapM (toText . const) acts
          r' <- toText $ const r
          return $ T.concat [ "Закрыть все ранее созданные по кейсу действия {"
                            , T.intercalate ", " acts'
                            , "} с результатом "
                            , r'
                            ]

    defer = textE "Отложить действие"

    finish = textE "Завершить обработку"

    proceed acts =
        TextE $
        T.append "Создать действия: " <$>
        T.intercalate ", " <$> mapM (toText . const) acts

    a *> b = TextE $ textBinary a b "" ", затем " ""


-- | TextE evaluator for DSL terms.
evalText :: TCtx -> TextE ty -> Text
evalText c t = runReader (toText t) c


-- | Show non-zero days, hours, minutes and seconds of a time
-- difference.
formatDiff :: NominalDiffTime -> Text
formatDiff nd' =
    let
        nd :: Int
        nd = round nd'
        totalDays = nd `div` days
        r1 = nd - totalDays * days
        totalHours = r1 `div` hours
        r2 = r1 - totalHours * hours
        totalMins = r2 `div` minutes
        totalSecs = (r2 - totalMins * minutes)
        labels = zip
                 [totalDays, totalHours, totalMins, totalSecs]
                 ["д", "ч", "м", "с"]
        nonZeros = filter (\(v, _) -> v /= 0) labels
    in
      T.pack $ concatMap (\(v, l) -> show v ++ l) nonZeros


-- | Produce a textual spec from a back office description.
backofficeText :: BackofficeSpec -> IMap -> Text
backofficeText spec iMap =
    T.unlines $
    ["ВХОДЫ:"] ++
    (indent . concat $ map fmtEntry $ fst spec) ++
    ["ДЕЙСТВИЯ:"] ++
    (indent . concat $ map fmtAction $ snd spec)
    where
      ctx = TCtx iMap
      indent :: [Text] -> [Text]
      indent = map ('\t' `T.cons`)
      fmtEntry e = [evalText ctx $ trigger e] ++ ["\n"]
      fmtAction a =
          [T.snoc (lkp (IBox $ aType a) iMap) ':'] ++
          indent
          ([ T.concat ["Время выполнения: ", evalText ctx $ due a]
           , T.concat ["Ответственность: ", evalText ctx $ assignment a]
           , "Результаты:" ] ++
           indent
           (Prelude.map (\(r, eff) ->
                         T.concat [ lkp (IBox r) iMap
                                  , ": "
                                  , evalText ctx eff
                                  ]) $
           outcomes a)) ++
          ["\n"]

{-# LANGUAGE ExistentialQuantification #-}

{-|

Text interpreter for Backoffice DSL terms.

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
import qualified Prelude as P ((>), (==), (||), (&&), const)

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
newtype TextE t = TextE (TCtx -> Text)


-- | Simple TextE constructor which leaves the context unchanged.
textE :: Text -> TextE t
textE t = TextE (P.const t)


-- | TextE constructor for trigger terms.
triggerText :: TextE a -> TextE v -> TextE t
triggerText field value =
    TextE $ \c ->
        T.concat [ "Когда "
                 , toText c field
                 , " приобретает значение "
                 , toText c value
                 ]


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
        TextE (\c -> T.concat [toText c t, " + ", formatDiff dt])
    before dt t =
        TextE (\c -> T.concat [toText c t, " - ", formatDiff dt])

    role r = TextE (\c -> T.append "Пользователи с ролью " $ toText c (const r))
    currentUserOr r =
        TextE $ \c ->
            T.append "Текущий пользователь и другие с ролью " $ toText c (const r)

    previousAction = textE "Предыдущее действие"

    userField     = textE . fieldDesc
    caseField     = textE . fieldDesc
    serviceField  = textE . fieldDesc
    serviceField' = textE . fieldDesc

    onCaseField a = triggerText (caseField a)
    onServiceField a = triggerText (serviceField a)
    onServiceField' a = triggerText (serviceField' a)

    not v =
        TextE (\c -> T.concat ["НЕ выполнено условие ", toText c v])
    a > b =
        TextE (\c -> T.concat [toText c a, " > ", toText c b])
    a == b =
        TextE (\c -> T.concat [toText c a, " равно ", toText c b])
    a && b =
        TextE (\c -> T.concat ["(", toText c a, ") и (", toText c b, ")"])
    a || b =
        TextE (\c -> T.concat ["(", toText c a, ") или (", toText c b, ")"])

    const v = TextE (lkp (IBox v) . identMap)

    oneOf val set =
        TextE $ \c ->
            T.concat [ toText c val
                     , " ∈ {"
                     , T.intercalate "," (map (toText c . const) set)
                     , "}"
                     ]

    switch conds ow =
        TextE $ \c ->
            let
                ppc (cond, act) =
                    T.concat ["Если ", toText c cond, ", то ", toText c act]
            in
              T.concat [ T.intercalate "; " $ Prelude.map ppc conds
                       , "; во всех других случаях — "
                       , toText c ow
                       ]

    setServiceField acc i =
        TextE $ \c ->
            T.concat [fieldDesc acc, " ← ", (toText c . const) i]

    sendSMS i =
        TextE $ \c ->
            T.concat ["Отправить SMS по шаблону ", (toText c . const) i]

    sendPSAMail = textE "Отправить письмо в PSA"

    sendDealerMail = textE "Отправить письмо дилеру"

    sendGenserMail = textE "Отправить письмо в Genser"

    closeWith acts r =
        TextE $ \c ->
            T.concat [ "Закрыть все ранее созданные по кейсу действия {"
                     , T.intercalate ", " $ map (toText c . const) acts
                     , "} с результатом "
                     , (toText c . const) r
                     ]

    defer = textE "Отложить действие"

    finish = textE "Завершить обработку"

    proceed acts =
        TextE $ \c ->
            T.append "Создать действия: " $
            T.intercalate ", " (map (toText c . const) acts)

    a *> b =
        TextE $ \c ->
        T.concat [toText c a, ", ", toText c b]


-- | Text evaluator for Backoffice DSL.
toText :: TCtx -> TextE v -> Text
toText ctx (TextE f) = f ctx


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
      fmtEntry e =
          [T.snoc (toText ctx $ trigger e) ':'] ++
          indent [toText ctx $ result e] ++
          ["\n"]
      fmtAction a =
          [lkp (IBox $ aType a) iMap] ++
          indent
          ([ T.concat ["Время выполнения: ", toText ctx $ due a]
           , T.concat ["Ответственность: ", toText ctx $ assignment a]
           , "Результаты:" ] ++
           indent
           (Prelude.map (\(r, eff) ->
                         T.concat [ lkp (IBox r) iMap
                                  , ": "
                                  , toText ctx eff
                                  ]) $
           outcomes a)) ++
          ["\n"]

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

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
import           Data.String
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable

import           Data.Model

import           Carma.Backoffice.DSL


-- | Structured text with indentation blocks.
data IndentedChunk = T Text
                   | NL
                   | IND IndentedText
                   deriving (Eq, Show)


type IndentedText = [IndentedChunk]


formatIndentedText :: Text
                   -- ^ Newline text.
                   -> Text
                   -- ^ Line prefix per indentation level.
                   -> IndentedText
                   -- ^ Source text.
                   -> Text
formatIndentedText newline indent = formatText1 0 ""
  where
    formatText1 _ acc [] = acc
    formatText1 l acc (c:cont) =
      case c of
        T t -> formatText1 l (acc `T.append` t) cont
        NL -> formatText1 l
              (T.concat [acc, newline, T.concat (l `replicate` indent)]) cont
        IND i ->
          formatText1 l
          (acc `T.append` formatText1 (l + 1) "" (NL:i)) (maybeNL cont)
          where
            -- Add newlines at start/end of indentation block if
            -- there's extra content on this indentation level
            -- (prevents superfluous newlines when a block body ends
            -- with deeper nesting, as in @IND [.., IND [..]]@)
            maybeNL [] = []
            maybeNL ls = NL:ls


-- | Format text, ignoring all indentation and newlines.
formatOneline :: IndentedText -> Text
formatOneline = foldl combine ""
  where
    combine acc NL = acc
    combine acc (T t) = acc `T.append` t
    combine acc (IND i) = acc `T.append` formatOneline i


-- | Convert an ident to text.
lkp :: IBox -> IMap -> Text
lkp k@(IBox k'@(Ident i)) env =
    maybe
    (T.pack $ show k')
    (\t -> T.concat [t, "#", T.pack $ show i])
    (Map.lookup k env)


-- | Text interpreter context.
data TCtx = TCtx { identMap :: IMap
                 }


-- | Text embedding for Backoffice DSL types.
newtype TextE t = TextE { toText :: Reader TCtx IndentedText }


-- | Simple TextE constructor which does not use the context.
textE :: Text -> TextE t
textE t = TextE (return [T t])


-- | Format binary function arguments (in one line).
textBinary :: TextE a
           -> TextE b
           -> Text
           -- ^ Opening bracket.
           -> Text
           -- ^ Argument separator.
           -> Text
           -- ^ Closing bracket.
           -> Reader TCtx IndentedText
textBinary a b open between close = do
  a' <- toText a
  b' <- toText b
  return $ [T open] ++ a' ++ [T between] ++ b' ++ [T close]


-- | TextE constructor for trigger terms.
triggerText :: TextE a
            -> TextE v
            -> TextE b
            -> Text
            -> Text
            -> Reader TCtx IndentedText
triggerText field value body open between = do
  trig' <- textBinary field value open between ""
  body' <- toText body
  return $ trig' ++ [T ": ", IND body']


scopeText :: IsString a => Scope -> a
scopeText InCase = "в кейсе"
scopeText InService = "в услуге"


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


instance Backoffice TextE where
    now = textE "Текущее время"
    since dt t =
        TextE $ (\c -> c ++ [T " + ", T $ formatDiff dt]) <$> toText t
    before dt t =
        TextE $ (\c -> c ++ [T " - ", T $ formatDiff dt]) <$> toText t

    nobody = textE "Без ответственного"

    currentUser = textE "Текущий пользователь"

    assigneeOfLast scope acts res =
        TextE $ do
          acts' <- mapM (toText . const) acts
          res' <- mapM toText res
          return $
            [ T "Пользователь, ответственный за последнее "
            , T $ scopeText scope
            , T " действие с типом {"
            ] ++
            intersperse (T " или ") (concat acts') ++
            [T "} в состоянии {"] ++
            intersperse (T " или ") (concat res') ++
            [T "}"]

    noResult = textE "Открыто"

    previousAction = textE "Предыдущее действие"

    userField     = textE . fieldDesc
    caseField     = textE . fieldDesc
    serviceField  = textE . fieldDesc

    onField f v body =
      TextE $
      triggerText (textE $ fieldDesc f) v body
      "Когда " " приобретает значение "

    insteadOf f v body =
      TextE $
      triggerText (textE $ fieldDesc f) v body
      "Вместо того, чтобы " " приобрело значение "

    not v = TextE $
            (\o -> T "НЕ выполнено условие ":o) <$> toText v

    a > b = TextE $ textBinary a b "" " > " ""

    a == b = TextE $ textBinary a b "" " равно " ""
    a && b = TextE $ textBinary a b "(" ") и (" ")"
    a || b = TextE $ textBinary a b "(" ") или (" ")"

    const v = TextE $ (\t -> [T t]) . lkp (IBox v) . identMap <$> ask

    just v = TextE $ (\t -> [T t]) . lkp (IBox v) . identMap <$> ask

    req v = TextE $ (++ [T "*"]) <$> toText v

    oneOf val set =
        TextE $ do
          val' <- toText val
          set' <- mapM (toText . const) set
          return $
            val' ++
            [T " ∈ {"] ++
            intersperse (T ", ") (concat set') ++
            [T "}"]

    switch conds ow =
        TextE $ do
          let ppc (cond, act) =
                do
                  cond' <- toText cond
                  act' <- toText act
                  return $ [T "Если "] ++ cond' ++ [T ", то:", IND act']
          ow' <- toText ow
          conds' <- mapM ppc conds
          return $
            concat conds' ++
            [T "Во всех других случаях: ", IND ow']

    setCaseField acc i =
        TextE $
        (\c -> [T $ fieldDesc acc, T " ← "] ++ c) <$> toText i

    setServiceField acc i =
        TextE $
        (\c -> [T $ fieldDesc acc, T " ← "] ++ c) <$> toText i

    sendSMS i =
        TextE $
        ([T "Отправить SMS по шаблону "] ++) <$> toText (const i)

    sendMail to =
      textE $ T.append "Отправить письмо " $ case to of
                                               PSA -> "в PSA"
                                               Dealer -> "дилеру"
                                               Genser -> "в Genser"

    when cond act =
        TextE $ do
          cond' <- toText cond
          act' <- toText act
          return $ [T "Если "] ++ cond' ++ [T ", то "] ++ act'

    closePrevious scope acts r =
        TextE $ do
          acts' <- mapM (toText . const) acts
          r' <- toText $ const r
          return $
            [ T "Закрыть все ранее созданные "
            , T $ scopeText scope
            , T " действия {"
            ] ++
            intersperse (T ", ") (concat acts') ++
            [T "} с результатом "] ++
            r'

    defer = textE "Отложить действие"

    proceed [] = textE "Завершить обработку"
    proceed acts =
        TextE $
          ([T "Создать действия: "] ++) <$>
            (intersperse (T ", ") . concat) <$> mapM (toText . const) acts

    a *> b =
      TextE $ do
        a' <- toText a
        b' <- toText b
        return $ a' ++ [NL] ++ b'


-- | TextE evaluator for DSL terms.
evalText :: TCtx -> TextE ty -> IndentedText
evalText c t = runReader (toText t) c


evalTextOneline :: TCtx -> TextE ty -> Text
evalTextOneline c t = formatOneline $ runReader (toText t) c


-- | Show non-zero days, hours, minutes and seconds of a time
-- difference.
formatDiff :: RealFrac a => a -> Text
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
backofficeText (BackofficeSpec{..}) iMap =
    formatIndentedText "\n" "    "
    [ T "ВХОДЫ:"
    , IND $ intercalate [NL] $
        map fmtEntry caseEntries ++ map fmtEntry svcEntries
    , NL
    , T "ДЕЙСТВИЯ:"
    , IND $ intercalate [NL] $ map fmtAction actNodes
    ]
    where
      ctx = TCtx iMap
      fmtEntry :: Entry m -> IndentedText
      fmtEntry e = evalText ctx (trigger e)
      fmtAction a =
        [ T $ T.snoc (lkp (IBox $ aType a) iMap) ':'
        , IND
          [ T "Время выполнения: "
          , IND $ evalText ctx $ due a
          , T "Для роли: "
          , IND $ evalText ctx $ targetRole a
          , T "Ответственность: "
          , IND $ evalText ctx $ assignment a
          , T "Результаты:"
          , IND
            (concatMap (\(r, eff) ->
                          [ T $ lkp (IBox r) iMap
                          , T ": "
                          , IND $ evalText ctx eff
                          ]) $
             outcomes a)
          ]
        ]

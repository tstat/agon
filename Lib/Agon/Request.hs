module Agon.Request
  ( parseRequests
  , toHttpClientRequest
  , Request
  ) where

import           Prelude hiding (lex)

import           Control.Applicative
import qualified Data.Aeson as Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import           Data.Char (isSpace)
import           Data.Foldable (foldl')
import qualified Data.List as List
import           Data.Maybe (listToMaybe)
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as Client
import           Network.HTTP.Types ( Method
                                    , methodGet
                                    , methodPost
                                    , methodPut
                                    , methodPatch
                                    , methodDelete
                                    , Header
                                    , hContentType
                                    , hAccept
                                    )
import           Network.URI (parseURI, URI)
import           System.Random (randomRIO)
import           Text.Earley

data Lexeme
  = Ident Text
  | StringLit Text
  deriving Show

fromLexeme :: Lexeme -> Text
fromLexeme (Ident txt) = txt
fromLexeme (StringLit txt) = txt

lex :: Text -> [Lexeme]
lex = ($ []) . φ id
  where
    φ :: ([Lexeme] -> [Lexeme]) -> Text -> ([Lexeme] -> [Lexeme])
    φ β α =
      let α' = T.dropWhile isSpace α
      in if T.null α'
      then β
      else case T.splitAt 1 α' of
        ("\'", α'') ->
          let δ = T.takeWhile (not . (== '\'')) α''
          in φ (β . ([StringLit δ] <>)) (T.drop (T.length δ + 1) α'')
        (α0, α1) ->
          let δ = T.takeWhile (not . isSpace) (α0 <> α1)
          in φ (β . ([Ident δ] <>)) (T.drop (T.length δ) (α0 <> α1))

methodParser :: Text -> Maybe Method
methodParser txt =
  case T.toLower txt of
    "get"    -> pure methodGet
    "post"   -> pure methodPost
    "put"    -> pure methodPut
    "patch"  -> pure methodPatch
    "delete" -> pure methodDelete
    _        -> fail "unrecognizable method"

hobParser :: Text -> Maybe HeaderOrBody
hobParser txt = literal <|> normal <|> header
  where
    normal :: Maybe HeaderOrBody
    normal = case T.split (== '=') txt of
      [k,"randInt"] -> Just $ SomeKeyValue k RandomInt
      [k,v] -> Just $ SomeKeyValue k (JSON $ Aeson.String v)
      _     -> Nothing

    literal :: Maybe HeaderOrBody
    literal = case T.splitOn ":=" txt of
      [k,v] -> SomeKeyValue k . JSON <$> Aeson.decodeStrict (TE.encodeUtf8 v)
      _     -> Nothing

    header :: Maybe HeaderOrBody
    header = case T.split (== ':') txt of
      [k,v] -> Just $ SomeHeader k (Literal v)
      _     -> Nothing

reqsGrammar :: Grammar r (Prod r Text Text [Request])
reqsGrammar = do
  rec
    g <- reqGrammar
    a <- rule $ (:) <$> g <*> (a <|> pure [])
  pure a

reqGrammar :: Grammar r (Prod r Text Text Request)
reqGrammar = do
  rec
    meth <- rule $ terminal methodParser
    uri <- rule $ terminal (parseURI . T.unpack)
    hob <- rule $ ((:) <$> terminal hobParser <*> hob) <|> pure []
  pure $ buildReq <$> meth <*> uri <*> hob

data HeaderOrBody
  = SomeHeader Text Value
  | SomeKeyValue Text Value
  deriving (Show)

data Request
  = Request
  { uri :: URI
  , method :: Method
  , headers :: [(Text, Value)]
  , body :: [(Text, Value)]
  } deriving (Show)

data Value
  = Literal Text
  | JSON Aeson.Value
  | RandomInt
  deriving (Show)

buildReq :: Method -> URI -> [HeaderOrBody] -> Request
buildReq meth uri hobs =
  let (hdrs, bdy) = gatherHeaderOrBody hobs
  in Request uri meth hdrs bdy
  where
    gatherHeaderOrBody :: Foldable φ => φ HeaderOrBody -> ([(Text, Value)], [(Text, Value)])
    gatherHeaderOrBody = foldl' ψ ([], [])
      where
        ψ (α,β) (SomeKeyValue k v) = (α, (k, v):β)
        ψ (α,β) (SomeHeader k v) = ((k,v):α, β)

toHttpClientRequest :: Request -> IO Client.Request
toHttpClientRequest (Request uri meth hdrs bdy) = do
  r <- Client.requestFromURI uri

  body <- traverse (uncurry resolveBody) bdy
  headers <- traverse (uncurry resolveHeader) hdrs
  pure r { Client.method = meth
         , Client.requestHeaders =
           case List.find (\(k,_) -> k == hContentType) headers of
             Just _ -> headers
             Nothing -> (hContentType, "application/json"):(hAccept, "application/json"):headers
         , Client.requestBody = if (meth == methodGet || null body)
           then Client.RequestBodyLBS ""
           else Client.RequestBodyLBS (Aeson.encode $ Aeson.object body)
         }
  where
    resolveHeader :: Text -> Value -> IO Header
    resolveHeader k v = (CI.mk (TE.encodeUtf8 k),) <$> fromValueHdr v

    resolveBody :: Text -> Value -> IO (Text, Aeson.Value)
    resolveBody k v = (k,) <$> fromValueBdy v

    fromValueBdy :: Value -> IO Aeson.Value
    fromValueBdy (Literal txt) = either (fail "Invalid json literal") pure
                                 (Aeson.eitherDecode $ BL.fromStrict $ TE.encodeUtf8 txt)
    fromValueBdy (JSON j) = pure j
    fromValueBdy RandomInt = Aeson.toJSON @ Int <$> randomRIO (0,1000)

    fromValueHdr :: Value -> IO ByteString
    fromValueHdr (Literal txt) = pure $ TE.encodeUtf8 txt
    fromValueHdr (JSON j) = pure . BL.toStrict $ Aeson.encode j
    fromValueHdr RandomInt = fromString . show @ Int <$> randomRIO (0,1000)

parseRequests :: Text -> Maybe [Request]
parseRequests
  = listToMaybe
  . fst
  . fullParses (parser reqsGrammar)
  . map fromLexeme
  . lex

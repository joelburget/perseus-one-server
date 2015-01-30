{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Codec.Picture
import Control.Applicative
import Control.Arrow
import Control.Monad ((>=>), mzero)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Hash.SHA1 (hashlazy)
import Data.Aeson (encode, ToJSON, FromJSON, parseJSON, genericParseJSON, toJSON, genericToJSON, Value(..), (.:))
import Data.Aeson.Types (defaultOptions)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Either
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (fromStrict)
import qualified Data.HashMap.Strict as H
import Data.Monoid
import Data.Unique
import qualified Data.Vector as V
import GHC.Generics
import Network.HTTP (getRequest, simpleHTTP, getResponseBody)
import Network.HTTP.Client (withManager, defaultManagerSettings)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method (StdMethod(OPTIONS))
import Network.HTTP.ReverseProxy
import Network.Wai (rawPathInfo, Application, Request)
import Network.Wai.Handler.Warp (defaultSettings, run)
import Network.Wai.Middleware.Static
import Network.Wai.Parse(fileContent)
import System.Process (readProcess)
import Text.Printf (printf)
import Web.Scotty

import Data.Data
import Data.Typeable

import GitStuff

-- schema
-- /
--     tags - json
--     exercise - json
--     items/
--         ajlfaslkdjfa (id) - json
--         ...
--     images/
--         lajfladsjfd (id) - image

data ProblemType = ProblemType
    { allAssessmentItems :: [Text]
    , name :: Text
    } deriving (Show, Generic)

instance FromJSON ProblemType where
    parseJSON = genericParseJSON defaultOptions

instance ToJSON ProblemType where
    toJSON = genericToJSON defaultOptions

data GetExercise = GetExercise
    { exercise :: [ProblemType]
    , items :: H.HashMap Text Text
    } deriving (Show, Generic)

-- instance FromJSON Exercise where
--     parseJSON = genericParseJSON defaultOptions

instance ToJSON GetExercise where
    toJSON = genericToJSON defaultOptions

newtype SaveExercise = SaveExercise { problem_types :: [ProblemType] }

instance FromJSON SaveExercise where
    parseJSON (Object v) = do
        (Array arr) <- v .: "problem_types"
        let probTys = V.toList $ V.map
                (\(Object ty) ->
                    let String name = ty H.! "name"
                        Array items = ty H.! "item"
                        items' = V.toList $ V.map
                            (\(Object i) ->
                                let String str = i H.! "id"
                                in str
                            )
                            items
                    in ProblemType items' name
                )
                arr
        return $ SaveExercise probTys

    parseJSON _ = mzero

get' :: RoutePattern -> ActionM () -> ScottyM ()
get' pat act = get pat $ do
    setHeader "Access-Control-Allow-Origin" "*"
    setHeader "Access-Control-Allow-Headers" "Cache-Control, Pragma, Origin, Authorization, Content-Type, X-Requested-With"
    act


-- requestIsP1Js :: RequestHeaders -> Bool
-- requestIsP1Js [] = False
-- requestIsP1Js ((hLocation, "/out.js"):_) = True
-- requestIsP1Js (_:hs) = requestIsP1Js hs


proxyingApp :: Application -> Request -> IO WaiProxyResponse
proxyingApp app req = do
    let path = rawPathInfo req
    return $ case path of
           "/out.js" -> WPRProxyDest $ ProxyDest "localhost" 8080
           _ -> WPRApplication app


main :: IO ()
main = do
    app <- p1App
    withManager defaultManagerSettings $ \m ->
        run 3000 (waiProxyTo (proxyingApp app) defaultOnExc m)


p1App :: IO Application
p1App = scottyApp $ do
    -- serve static files from the working dir
    middleware static

    get "/" $ do
        setHeader "Access-Control-Allow-Methods" "GET, PUT, POST, DELETE"
        file "index.html"

    get' "/api/tags" $ json ([] :: [Int])

    get' "/api/fetch-exercise" $ do
        maybeEx <- liftIO $ getBlob "exercise"
        liftIO $ print maybeEx
        -- let maybeEx' = decode =<< maybeEx
        let exBS = fromMaybe (BL.toStrict $ encode defaultEx) maybeEx
            -- Just parsed = decodeStrict' exBS

        -- fetchedItems <- liftIO $ forM (items parsed) $ \item ->
        --     getBlob item

        let m :: H.HashMap Text Text
            m = H.fromList
                [ ("exercise", decodeUtf8 exBS)
                -- , ("items", fetchedItems')
                ]
        -- json m
        text $ fromStrict $ decodeUtf8 exBS
        setHeader "Content-Type" "application/json"

    put "/api/save-exercise" $ do
        SaveExercise problemTypes <- jsonData
        liftIO $ makeBlob ("exercise" :: Text) (HoldJson problemTypes)

    post "/api/save-item" $ doItemStuff =<< generateName

    put "/api/save-item/:id" $ doItemStuff =<< param "id"

    post "/api/upload-image" $ do
        Just fileInfo <- H.lookup "upload_image" <$> H.fromList <$> files
        -- TODO use fileContentType to figure out what this is?
        let img = BL.toStrict $ fileContent fileInfo
        case recognizeType img of
            Nothing -> text "that is not an image"
            Just ty -> doImageStuff img ty

    post "/api/relocate-image" $ do
        url <- param "url"
        img <- liftIO $ fromString <$> downloadImage url
        case recognizeType img of
            Nothing -> text "that is not an image"
            Just ty -> doImageStuff img ty


defaultEx :: GetExercise
defaultEx = GetExercise [] (H.fromList [])


doItemStuff :: Text -> ActionM ()
doItemStuff itemName = do
    (newItemB, newItemT) <- body'
    liftIO $ print itemName
    liftIO $ makeBlob itemName newItemT
    desc <- itemDescriptor itemName newItemB
    json desc


-- name and save the image, then return the url in json
doImageStuff :: BS.ByteString -> Text -> ActionM ()
doImageStuff img ty = do
    imageName <- generateName
    let name = imageName <> "." <> ty
    liftIO $ makeBlob name img
    let m :: H.HashMap Text Text
        m = H.fromList [("url", "/repo/" <> name)]
    json m


body' :: ActionM (ByteString, Text)
body' = do
    b <- body
    return (b, decodeUtf8 $ BL.toStrict b)


itemDescriptor :: MonadIO m => Text -> ByteString -> m (H.HashMap Text Text)
itemDescriptor name contents = do
    let sha = toHex $ hashlazy contents
    return $ H.fromList
        [ ("id", name)
        , ("sha", sha)
        , ("publishedSha", sha)
        ]


toHex :: BS.ByteString -> Text
toHex bytes = fromString $ BS.unpack bytes >>= printf "%02x"


recognizeType :: BS.ByteString -> Maybe Text
recognizeType img =
    let -- get the first (Just _) in a list
        first = getFirst . mconcat . map First

        -- try decoding the image with every decoder
        decoders =
            [ decodes "jpg" (decodeJpeg img)
            , decodes "png" (decodePng img)
            , decodes "gif" (decodeGif img)
            ]
        decodes ty decoded = if isRight decoded then Just ty else Nothing

    in first decoders


downloadImage :: String -> IO String
downloadImage = (getRequest >>> simpleHTTP) >=> getResponseBody


generateName :: MonadIO m => m Text
generateName = liftIO $ (T.pack . show . hashUnique) <$> newUnique

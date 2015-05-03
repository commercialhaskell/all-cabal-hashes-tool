{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}
import           ClassyPrelude.Conduit
import qualified Codec.Archive.Tar           as Tar
import           Crypto.Hash                 (HashAlgorithm, MD5 (..),
                                              SHA1 (..), SHA256 (..),
                                              SHA512 (..), Skein512_512 (..))
import           Crypto.Hash.Conduit         (sinkHash)
import           Crypto.Hash.Types           (Digest (Digest))
import           Data.Aeson                  (FromJSON (..), ToJSON (..),
                                              eitherDecode', encode, object,
                                              withObject, (.:), (.=))
import qualified Data.ByteString.Base16      as B16
import           Data.Conduit.Lazy           (lazyConsume)
import           Data.Conduit.Zlib           (ungzip)
import           Filesystem                  (createTree, isFile)
import           Filesystem.Path             (dropExtension, parent)
import           Network.HTTP.Client.Conduit (HasHttpManager, HttpException (StatusCodeException),
                                              checkStatus, parseUrl,
                                              responseBody, responseCookieJar,
                                              responseHeaders, responseStatus,
                                              withManager, withResponse)
import           Network.HTTP.Types          (statusCode)

type M env m =
    ( HasHttpManager env
    , MonadThrow m
    , MonadBaseControl IO m
    , MonadReader env m
    , MonadIO m
    )

main :: IO ()
main = withManager $ do
    withIndex $ \src -> do
        entries <- Tar.read . fromChunks <$> lazyConsume src
        sourceEntries entries
            $$ mapMC handleEntry
            =$ limitTo 5000

withIndex :: M env m
          => (Source m ByteString -> m a)
          -> m a
withIndex inner =
    withResponse "https://hackage.haskell.org/packages/archive/00-index.tar.gz"
        $ \res -> inner $ responseBody res =$= ungzip
{-
withIndex inner = bracket
    (liftIO $ openBinaryFile "/home/vagrant/.cabal/packages/hackage.haskell.org/00-index.tar.gz" ReadMode)
    (liftIO . hClose)
    (\h -> inner $ sourceHandle h =$= ungzip)
        -}

sourceEntries :: (Exception e, MonadThrow m) => Tar.Entries e -> Source m Tar.Entry
sourceEntries Tar.Done = return ()
sourceEntries (Tar.Next e rest) = yield e >> sourceEntries rest
sourceEntries (Tar.Fail e) = throwM e

limitTo :: MonadIO m => Int -> Sink Int m ()
limitTo total =
    loop (0 :: Int)
  where
    loop cnt
        | cnt >= total = putStrLn $ "Limiting to " ++ tshow total ++ " downloads in one go"
        | otherwise = await >>= maybe (return ()) (loop . (cnt +))

handleEntry :: M env m => Tar.Entry -> m Int
handleEntry entry
    | Just (pkg, ver) <- toPkgVer $ Tar.entryPath entry
    , Tar.NormalFile lbs _ <- Tar.entryContent entry = do
        exists <- liftIO $ isFile jsonfp
        (downloadTry, mpackage) <- if exists
            then do
                eres <- eitherDecode' <$> readFile jsonfp
                case eres of
                    Left e -> error $ concat
                        [ "Could not parse "
                        , fpToString jsonfp
                        , ": "
                        , e
                        ]
                    Right x -> return (0, Just x)
            else do
                mpackage <- computePackage pkg ver
                forM_ mpackage $ \package -> do
                    liftIO $ createTree $ parent jsonfp
                    writeFile jsonfp $ encode package
                return (1, mpackage)
        forM_ mpackage $ \package -> writeFile cabalfp $ concat
            [ lbs
            , "\n\n"
            , encodeUtf8 $ builderToLazy $ fromPackage package
            ]
        return downloadTry
  where
    cabalfp = fromString $ Tar.entryPath entry
    jsonfp = dropExtension cabalfp <.> "json"
handleEntry _ = return 0

data Package = Package
    { _packageHashes    :: Map Text Text
    , _packageLocations :: [Text] -- ^ why no ToJSON/FromJSON for Vector?
    }
    deriving Show
instance ToJSON Package where
    toJSON (Package h l) = object
        [ "package-hashes" .= h
        , "package-locations" .= l
        ]
instance FromJSON Package where
    parseJSON = withObject "Package" $ \o -> Package
        <$> o .: "package-hashes"
        <*> o .: "package-locations"

fromPackage :: Package -> TextBuilder
fromPackage (Package hashes locations) =
    fromHashes hashes ++ "\n" ++ fromLocations locations ++ "\n"

fromHashes :: Map Text Text -> TextBuilder
fromHashes =
    ("package-hashes:\n" ++) . foldMap go . mapToList
  where
    go (name, val) = "    " ++ toBuilder name ++ ":" ++ toBuilder val ++ "\n"

fromLocations :: [Text] -> TextBuilder
fromLocations =
    ("package-locations:\n" ++) . foldMap go
  where
    go t = "    " ++ toBuilder t ++ "\n"

toPkgVer :: String -> Maybe (Text, Text)
toPkgVer s@(stripSuffix ".cabal" . pack -> Just t0)
    | pkg == pkg2 = Just (pkg, ver)
    | otherwise = error $ "toPkgVer: could not parse " ++ s
  where
    (pkg, uncons -> Just ('/', t1)) = break (== '/') t0
    (ver, uncons -> Just ('/', pkg2)) = break (== '/') t1
toPkgVer _ = Nothing

computePackage :: M env m
               => Text -- ^ package
               -> Text -- ^ version
               -> m (Maybe Package)
computePackage pkg ver = do
    putStrLn $ "Computing package information for: " ++ pack pkgver
    s3req <- parseUrl s3url
    hackagereq <- parseUrl hackageurl

    mhashes <- withResponse s3req { checkStatus = \_ _ _ -> Nothing } $ \resS3 -> do
        case statusCode $ responseStatus resS3 of
            200 -> do
                hashesS3 <- responseBody resS3 $$ hashesSink
                hashesHackage <- withResponse hackagereq $ \res -> responseBody res $$ hashesSink

                when (hashesS3 /= hashesHackage) $
                    error $ "Mismatched hashes between S3 and Hackage: " ++ show (pkg, ver, hashesS3, hashesHackage)

                return $ Just hashesS3
            403 -> do
                -- File not yet uploaded to S3
                putStrLn $ "Skipping file not yet on S3: " ++ pack pkgver
                return Nothing
            _ -> throwM $ StatusCodeException
                (responseStatus resS3)
                (responseHeaders resS3)
                (responseCookieJar resS3)

    let locations =
            [ pack hackageurl
            , pack s3url
            ]
    return $ flip Package locations <$> mhashes
  where
    pkgver = unpack pkg ++ '-' : unpack ver
    hackageurl = concat
        [ "https://hackage.haskell.org/package/"
        , pkgver
        , "/"
        , pkgver
        , ".tar.gz"
        ]
    s3url = concat
        [ "https://s3.amazonaws.com/hackage.fpcomplete.com/package/"
        , pkgver
        , ".tar.gz"
        ]
    hashesSink = getZipSink $ fmap unions $ sequenceA
        [ mkSink SHA1
        , mkSink SHA256
        , mkSink SHA512
        , mkSink Skein512_512
        , mkSink MD5
        ]

mkSink :: (Monad m, Show hash, HashAlgorithm hash) => hash -> ZipSink ByteString m (Map Text Text)
mkSink ha = ZipSink $ do
    digest <- sinkHash
    return $ singletonMap (tshow ha) $ unDigest ha digest

unDigest :: hash -> Digest hash -> Text
unDigest _ (Digest bs) = decodeUtf8 $ B16.encode bs

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import qualified Network.TLS as T
import qualified Network.TLS.Extra as TE
import Network.Socket hiding (send, recv)
import Data.Default.Class (def)
import Control.Exception (IOException, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Maybe (fromMaybe, listToMaybe)
import System.Environment (getArgs)
import Control.Concurrent (forkIO)

getSocket :: IO Socket
getSocket = socket AF_INET Stream defaultProtocol

tlsPort :: IO PortNumber
tlsPort = getArgs >>= pure . fromMaybe 8443 . listToMaybe . (map read) 

recv :: T.Context -> IO (Either IOException BS.ByteString)
recv ctx = try $ T.recvData ctx

send :: T.Context
     -> [BS.ByteString]
     -> IO (Either IOException ())
send ctx bs = try $ T.sendData ctx $ L8.fromChunks $ bs

pong ctx = do
  res <- recv ctx
  case Right "ping" == res of
    False -> T.contextClose ctx
    True  -> do
      req <- send ctx $ [ "pong" ]
      case Right () == req of
        False -> T.contextClose ctx
        True  -> pong ctx

spawn (sock, _) creds = do
  ctx <- T.contextNew sock $ para creds
  _ <- T.handshake ctx
  pong ctx
  where para x509 = def
                    { T.serverWantClientCert = False
                    , T.serverShared         = shared
                    , T.serverSupported      = supported
                    }
          where shared = def { T.sharedCredentials = x509 }
                supported = def { T.supportedVersions = [T.TLS12]
                                , T.supportedCiphers  = ciphers
                                }
                ciphers = [ TE.cipher_AES128_SHA1
                          , TE.cipher_AES256_SHA1
                          , TE.cipher_RC4_128_MD5
                          , TE.cipher_RC4_128_SHA1
                          ]

loop sock (Right creds) = do
  conn <- accept $ sock
  putStrLn $ ("Connected to: " ++) $ show $ snd $ conn
  _ <- forkIO $ spawn conn $ T.Credentials [creds]
  loop sock $ Right creds
loop _ (Left msg) =
  putStrLn $ msg

server :: IO ()
server = do
  port <- tlsPort
  sock <- getSocket
  _ <- bind sock $ SockAddrInet port $ tupleToHostAddress (127, 0, 0, 1)
  _ <- listen sock 256
  putStrLn $ "Listening on port " ++ show port

main :: IO ()
main = scotty 3000 $ do
  get "/" (html "Hello, World!")

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Network.TLS
import Network.Socket
import Data.Default.Class (def)

getPort :: IO Int
getPort = pure 8443

addrProtocol :: ProtocolNumber
addrProtocol = 0

getSocket :: IO Socket
getSocket = socket AF_INET Stream addrProtocol

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
  port <- getPort
  sock <- getSocket
  _ <- bind sock $ SockAddrInet port $ tupleToHostAddress (127, 0, 0, 1)
  _ <- listen sock 256
  putStrLn $ "Listening on port " ++ show port

main :: IO ()
main = scotty 3000 $ do
  get "/" (html "Hello, World!")

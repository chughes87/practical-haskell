data ConnType = TCP | UDP
data UseProxy = NoProxy | Proxy String
data TimeOut = NoTimeOut | Timeout Integer

data ConnOptions = ConnOptions {
  connType :: ConnType,
  connSpeed :: Integer,
  connProxy :: UseProxy,
  connCaching :: Bool,
  connKeepAlive :: Bool,
  connTimeOut :: TimeOut
}

connect' :: String -> ConnOptions -> Connection

module Chapter2.DataTypes (ConnOptions(), connDefault) where


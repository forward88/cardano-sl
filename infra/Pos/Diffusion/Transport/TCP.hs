-- | Convenient TCP transport acquire and release with common configuration
-- options.

module Pos.Diffusion.Transport.TCP
    ( bracketTransportTCP
    ) where

import           Universum

import           Data.Time.Units (Microsecond)

import           Network.QDisc.Fair (fairQDisc)
import qualified Network.Transport as NT (Transport, closeTransport)
import qualified Network.Transport.TCP as TCP

bracketTransportTCP
    :: ( )
    => Microsecond
    -> TCP.TCPAddr
    -> (SomeException -> IO ())
    -> (NT.Transport -> IO a)
    -> IO a
bracketTransportTCP connectionTimeout tcpAddr handleServerException k = bracket
    (createTransportTCP connectionTimeout tcpAddr handleServerException)
    snd
    (k . fst)

createTransportTCP
    :: ( )
    => Microsecond -- ^ Connection timeout
    -> TCP.TCPAddr
    -> (SomeException -> IO ()) -- ^ What to do if there's an exception when
                                -- accepting a new connection. Throwing it
                                -- will kill the server
    -> IO (NT.Transport, IO ())
createTransportTCP connectionTimeout addrInfo handleServerException = do
    let tcpParams =
            (TCP.defaultTCPParameters
             { TCP.transportConnectTimeout = Just $ fromIntegral connectionTimeout
             , TCP.tcpNewQDisc = fairQDisc $ \_ -> return Nothing
             -- Will check the peer's claimed host against the observed host
             -- when new connections are made. This prevents an easy denial
             -- of service attack.
             , TCP.tcpCheckPeerHost = True
             , TCP.tcpServerExceptionHandler = handleServerException
             })
    transportE <- TCP.createTransport addrInfo tcpParams
    case transportE of
        Left e -> throwM e
        Right transport -> return (transport, NT.closeTransport transport)

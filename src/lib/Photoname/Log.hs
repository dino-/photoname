module Photoname.Log
  ( initLogging
  , lname
  , logTest

  -- Re-exported from System.Log
  , debugM, infoM, noticeM, warningM, errorM, criticalM, alertM, emergencyM
  )
  where

import System.IO ( Handle, stdout )
import System.Log.Handler.Simple ( GenericHandler, streamHandler )
import System.Log.Logger

import Photoname.Common ( Verbosity (Quiet, Verbose) )


lname :: String
lname = rootLoggerName


initLogging :: Verbosity -> IO ()
initLogging verbosity = do
  updateGlobalLogger lname . setHandlers =<< handlers verbosity
  case verbosity of
    Quiet -> pure ()
    Verbose loggerLevel -> updateGlobalLogger lname $ setLevel loggerLevel


handlers :: Verbosity -> IO [GenericHandler Handle]
handlers Quiet  = pure []
handlers (Verbose _) = sequence [streamHandler stdout DEBUG]


-- Test function to generate every kind of log message
logTest :: IO ()
logTest = do
  debugM     lname "log test message DEBUG 1 of 8"
  infoM      lname "log test message INFO 2 of 8"
  noticeM    lname "log test message NOTICE 3 of 8"
  warningM   lname "log test message WARNING 4 of 8"
  errorM     lname "log test message ERROR 5 of 8"
  criticalM  lname "log test message CRITICAL 6 of 8"
  alertM     lname "log test message ALERT 7 of 8"
  emergencyM lname "log test message EMERGENCY 8 of 8"

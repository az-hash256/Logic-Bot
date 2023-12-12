{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when, void)
import UnliftIO.Concurrent

import qualified Data.Text.IO as TIO
import Discord
import Discord.Types
import qualified Discord.Requests as R

import Data.Text
import Data.Maybe

import ValidityChecker

main :: IO ()
main = do
    userFacingError <- runDiscord $ Discord.def
             { discordToken = "Your Personal Token"
             , discordOnEvent = eventHandler
             , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
             } -- if you see OnLog error, post in the discord / open an issue

    TIO.putStrLn userFacingError
    -- userFacingError is an unrecoverable error
    -- put normal 'cleanup' code in discordOnEnd (see examples)

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
    MessageCreate m -> when (isEval m && not (fromBot m)) $ do
        let argText = stripEvalPrefix (messageContent m)
            arg = parseArg (unpack argText)
            validity = isMaybeValid arg
            idnum = show $ userId $ messageAuthor m
            mention = "<@"++ idnum ++ ">"
         in do
            case arg of
                Just actualArg -> do
			case maxVariables actualArg of
			   True -> do
				void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "man_facepalming")
                                void $ restCall (R.CreateMessage (messageChannelId m) (pack ("<@"++ idnum ++ ">" ++ " Your argument has more variables than 16 variables. Use less please.")))
			   False -> do 
				if validity
                                        then void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "white_check_mark")
                                        else void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "x")
                                threadDelay (2 * 10^6)
                                void $ restCall (R.CreateMessage (messageChannelId m) (if validity then pack (mention ++ " The argument you gave is Valid.") else pack (mention ++ " The argument you gave is invalid.")))
                Nothing -> do
                   void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "man_facepalming")
                   void $ restCall (R.CreateMessage (messageChannelId m) (pack ("<@"++ idnum ++ ">" ++ " The argument you gave isn't well-formed.")))
    _ -> return ()


-- Helper Functions
fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isEval :: Message -> Bool
isEval = ("eval" `isPrefixOf`) . messageContent

isSuperSecret :: Message -> Bool
isSuperSecret = ("supersecret shhh" `isInfixOf`) . messageContent

stripEvalPrefix :: Text -> Text
stripEvalPrefix = maybe "" id . stripPrefix "eval "

maxVariables :: Arg -> Bool
maxVariables arg = Prelude.length (getArgVariables arg) > 16

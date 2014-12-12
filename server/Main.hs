-- Copyright © 2014 Garrison Jensen
-- License
-- This code and text are dedicated to the public domain.
-- You can copy, modify, distribute and perform the work,
-- even for commercial purposes, all without asking permission.

import Control.Concurrent
import Control.Monad
import qualified Control.Exception as E
import System.IO
import Data.List
import Network.CGI.Protocol
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket, PortNumber)
import ServerState 
import ServerBot


--------------------------------------------------------------------------------
--  Main

main :: IO ()
main = withSocketsDo $ do
    -- Create a Socket and listen on port
    sock <- listenOn port
    -- Create a new server state
    state <- newEmptyServerState
    -- Start ServerBot
    -- ServerBot will use the PING command to check for unresponsive users.
    _ <- forkIO $ serverBot state 
    -- Loop forever
    loop sock state


--------------------------------------------------------------------------------
--  Loop
--  Loop forever listening for new connections

loop :: Socket -> MVar ServerState -> IO b 
loop sock state = do
    -- Accept new connection
    (hndl,_, p) <- accept sock
    -- Create New user
    _ <- addUser (hndl, p) state
    -- Fork a new thread for user
    -- If exception is caught, remove user from state
    _ <- forkIO $ E.catch (body hndl state) $ disconnected p
    -- Continue loop
    loop sock state
  where
    addUser :: (Handle, PortNumber) -> MVar ServerState -> IO ()
    addUser hndl stt = do 
                s <- takeMVar stt
                putMVar stt $ s{users=User "" False hndl:users s}
    disconnected prt ex = do 
                      removeUserWithPort prt state
                      hPutStrLn stderr $ "Disconnected: " ++ err
                    where err = show (ex :: E.IOException)


--------------------------------------------------------------------------------
--  Body
--  Each user will loop forever on this function.
--  Users are identified by their socket handle.

body :: Handle -> MVar ServerState -> IO ()
body hndle state = do
    line <- hGetLine hndle
    -- Each line is parsed by getCmd. Depending on which command received, a 
    -- handle function will be dispatched to complete that command. Each handle 
    -- function is an IO function and can perform an arbitrary number of 
    -- IO actions. In addition, a response will be returned that should be sent
    -- to the commanding user.
    response <- case getCmd line of
                Just (NICK, msg)    -> handleNick hndle msg state
                Just (PART, msg)    -> handlePart hndle msg state
                Just (JOIN, msg)    -> handleJoin hndle msg state
                Just (NAMES, msg)   -> handleNames hndle msg state
                Just (LIST, msg)    -> handleList hndle msg state
                Just (PRIVMSG, msg) -> handlePrivMsg hndle msg state
                Just (PING, msg)    -> handlePING hndle msg state
                Just (PONG, msg)    -> handlePONG hndle msg state
                Nothing             -> return "Command Error"
    -- Send response
    _ <- hPutStrLn hndle response 
    -- Loop 
    body hndle state


--------------------------------------------------------------------------------
--  Handle Functions
--  A handle function will be dispatched for every command received. Each 
--  handle function will perform an arbitrary number of IO actions. Also, it
--  may lock the ServerState for a period of time. In addition, each function
--  will return a message that should be sent to the commanding user.

handleNick :: Handle -> Message -> MVar ServerState -> IO Message
handleNick hndl msg mvState = do
        state <- takeMVar mvState
        case updateUser state of
              Just newState -> do 
                        putMVar mvState newState
                        return "Nick Updated"
              Nothing -> do 
                        putMVar mvState state
                        return "There was some error updating nick"
       where
        updateUser state = do
                    u <- handleToUser state hndl
                    case nickToUser state msg of
                        Just _  -> Nothing -- Nick already exists
                        Nothing -> return $ updateNick u msg state


handleJoin :: Handle -> Message -> MVar ServerState -> IO Message
handleJoin hndl msg mvState = do
          state <- takeMVar mvState
          case addUsrTRm state of
              Just newState -> do 
                        putMVar mvState newState
                        return $ ":Successfully joined " ++ msg ++ ":"
              Nothing -> do 
                        putMVar mvState state
                        return "There was some error joining room"
      where
        addUsrTRm s = do
                        u <- handleToUser s hndl
                        -- Create a list of actions that take a server
                        -- and return a server with that action completed.
                        let listOfActions = map (addUserToRoom u) $ words msg
                        -- Complete all of the actions.
                        foldM (\st r -> r st) s listOfActions

handlePart :: Handle -> Message -> MVar ServerState -> IO Message
handlePart hndl msg mvState = do
          state <- takeMVar mvState
          case rmvUsrFrmRm state of
              Just newState -> do 
                        putMVar mvState newState
                        return $ ":Successfully left " ++ msg ++ ":"
              Nothing -> do 
                        putMVar mvState state
                        return "There was some error leaving room"
      where
        rmvUsrFrmRm s = do
                        u <- handleToUser s hndl
                        removeUserFromRoom u msg s

handleNames :: Handle -> Message -> MVar ServerState -> IO Message
handleNames _ msg mvState = do
                state <- readMVar mvState
                return $ unwords $ case getRoom msg state of
                                        Just r ->  nicks r
                                        Nothing -> map nick $ users state
                    
handleList :: Handle -> Message -> MVar ServerState -> IO Message
handleList _ _ mvState = do
                state <- readMVar mvState
                return $ unwords $ map name $ rooms state
                  
handlePrivMsg :: Handle -> Message -> MVar ServerState -> IO Message
handlePrivMsg hndl msg mvState = do 
                state <- readMVar mvState
                case find (findUser hndl) $ users state of
                    Just nck -> case prefix of
                            ('#':_) -> messageRoom state nck prefix message
                            ('&':_) -> messageRoom state nck prefix message
                            _       -> messageUser state prefix message
                    Nothing -> return "Failed to send message"
                
              where
                prefix = head (words msg)
                message = unwords $ tail $ words msg
                messageRoom s sender r m = case find (\x -> name x == r) (rooms s) of 
                                      Just x -> do 
                                            mapM_ (sendUser s (nick sender) ("PRIVMSG " ++ prefix) m) (nicks x)
                                            return ":Message sent:"
                                      Nothing -> return "Failed to send message"
                sendUser state sender prfx mess nickToSend = case nickToUser state nickToSend of
                                                                Just usr -> do
                                                                    let (h,_) = handle usr
                                                                    hPutStrLn h $ ':':sender ++ " " ++ prfx ++ ": " ++ mess
                                                                Nothing -> return ()
                messageUser s u m = case nickToUser s u of
                                    Just usr -> do 
                                        let (h,_) = handle usr
                                        _ <- hPutStrLn h $ ':':nick usr ++ " :" ++ m
                                        return ""
                                    Nothing -> return "Failed to send message to user"
                                  
handlePING :: Handle -> Message -> MVar ServerState -> IO Message
handlePING _ _ _ = return ":I PING, you PONG:"

handlePONG :: Handle -> Message -> MVar ServerState -> IO Message
handlePONG hndl _ mvState = do
                  state <- takeMVar mvState
                  case handleToUser state hndl of
                      Just u -> do 
                              putMVar mvState $ state{users=u{dead=False}:removeUserFromUserList u (users state)}
                              return ":Thanks for the PONG:"
                      Nothing -> return "Failed to accept PONG"


--------------------------------------------------------------------------------
--  Functions for Parsing Messages

getCmd :: String -> Maybe (Cmd, Message)
getCmd msg = do
            cmd <- maybeRead (head msgLines)
            validateMessage (cmd, message)
        where msgLines = words msg
              message = unwords $ tail msgLines


validateMessage :: (Cmd, Message) -> Maybe (Cmd, Message)
validateMessage m@(cmd, msg) = case cmd of
                        NICK  -> if not (null msg) then Just m else Nothing
                        JOIN  -> case msg of
                            ('#':_) -> Just m
                            ('&':_) -> Just m
                            _       -> Nothing
                        PART  -> Just (cmd, msg)
                        NAMES  -> Just (cmd, msg)
                        LIST  -> Just (cmd, msg)
                        PRIVMSG  -> if not (null msg) then Just m else Nothing
                        PING  -> Just m
                        PONG  -> Just m

--------------------------------------------------------------------------------
--  Helper Functions

handleToUser :: ServerState -> Handle -> Maybe User
handleToUser state hndl = find (findUser hndl) $ users state

nickToUser :: ServerState -> String -> Maybe User
nickToUser state nck = find (\a -> nick a == nck) (users state)

removeUserWithPort :: PortNumber -> MVar ServerState -> IO ()
removeUserWithPort uPort mvState= do
                    state <- takeMVar mvState
                    let newState = case usr state of
                                Just u  -> removeUserFromState u state
                                Nothing -> state
                    putMVar mvState newState
                  where
                    usr s = find (\u -> (snd . handle) u == uPort) $ users s

--------------------------------------------------------------------------------
--  Constants

port :: PortID
port = PortNumber 5002

newEmptyServerState :: IO (MVar ServerState)
newEmptyServerState = newMVar $ ServerState [] []

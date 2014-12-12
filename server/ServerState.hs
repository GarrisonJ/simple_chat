-- Copyright © 2014 Garrison Jensen
-- License
-- This code and text are dedicated to the public domain.
-- You can copy, modify, distribute and perform the work,
-- even for commercial purposes, all without asking permission.

module ServerState where

import Data.List
import Network.CGI.Protocol
import Network 
import System.IO

--------------------------------------------------------------------------------
--  Data Types

data ServerState = ServerState { rooms :: [Room] 
                               , users :: [User] 
                               }

data User = User { nick :: String
                 , dead :: Bool
                 , handle :: (Handle,  PortNumber) 
                 } deriving (Eq, Show)

data Room = Room  { name :: String
                  , nicks :: [String] 
                  } deriving (Eq, Show)

data Cmd = NICK | JOIN | PART | NAMES | LIST | PRIVMSG | PING | PONG deriving (Read)

type Message = String

--------------------------------------------------------------------------------
--  Functions
--  In general, pattern matching should be used to modify state. However, I've 
--  created functions to handle common and complex actions.

--------------------------------------------------------------------------------
--  State Modifiers 

removeUserFromState :: User -> ServerState -> ServerState
removeUserFromState u s = ServerState (removeUserFromRooms u r) (removeUserFromUserList u ul)
                        where
                          r = rooms s
                          ul = users s

addUserToRoom :: User -> String -> ServerState -> Maybe ServerState
addUserToRoom u roomName s = case getRoom roomName s of
                    Just r -> do 
                            n <- case nick u of
                                    "" -> Nothing
                                    a -> Just a
                            return $ if n `elem` nicks r
                                        then s
                                        else s{rooms=Room roomName (n:nicks r):delete r (rooms s)}
                    Nothing -> do
                            n <- case nick u of
                                    "" -> Nothing
                                    a -> Just a
                            return $ s{rooms=Room roomName [n]:rooms s}

removeUserFromRoom :: User -> String -> ServerState -> Maybe ServerState
removeUserFromRoom u roomName s = do
                                  r <- getRoom roomName s
                                  return $ case delete (nick u) (nicks r) of
                                            []   -> s{rooms=delete r (rooms s)}
                                            newr -> s{rooms=Room roomName newr:delete r (rooms s)}

removeUserFromRooms :: User -> [Room] -> [Room]
removeUserFromRooms _ [] = []
removeUserFromRooms u (r:rs) = case delete (nick u) (nicks r) of
                                    []   -> removeUserFromRooms u rs
                                    newr -> r{nicks=newr}:removeUserFromRooms u rs

removeUserFromUserList :: User -> [User] -> [User]
removeUserFromUserList = delete

updateNick :: User -> String -> ServerState -> ServerState
updateNick u newn state = state{users=newUser:delete u (users state), rooms=changeNick (nick u) newn (rooms state)}
                    where
                        newUser = u{nick=newn}
                        changeNick _ _ [] = []
                        changeNick n newN (r:rs) = r{nicks=replace n newN (nicks r)}:changeNick n newN rs
                      
--------------------------------------------------------------------------------
--  State Accessors

findUser :: Handle -> User -> Bool                                
findUser h (User _ _ (handl,_)) = handl == h

getRoom :: String -> ServerState -> Maybe Room
getRoom n s = find ((== n) . name) $ rooms s
          
roomsJoined :: User -> ServerState -> [Room]
roomsJoined u s = filter (\x -> nick u `elem` nicks x) $ rooms s

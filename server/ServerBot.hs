-- Copyright © 2014 Garrison Jensen
-- License
-- This code and text are dedicated to the public domain.
-- You can copy, modify, distribute and perform the work,
-- even for commercial purposes, all without asking permission.

module ServerBot where

import ServerState 
import System.IO
import Control.Concurrent

-- At most every 10 seconds, a PING message will be sent to every user.
-- If user does not repspond in 10 seconds, that user's connection will be closed.
serverBot :: MVar ServerState -> IO b
serverBot mvState = do
                  -- Wait 10 seconds
                  threadDelay 10000000
                  
                  -- Take state
                  -- Ping every user
                  -- Set every user to dead
                  -- Put state
                  firstState <- takeMVar mvState
                  everyUserDead <- pingEveryone firstState
                  putMVar mvState everyUserDead

                  -- Wait 10 seconds to give clients a chance to respond.
                  -- Responses are handled by the handlePONG function.
                  threadDelay 10000000

                  -- Take state
                  -- Boot every user who is still dead.
                  -- Put state
                  secondState <- takeMVar mvState
                  newState <- bootDeadUsers secondState
                  putMVar mvState newState

                  -- Loop
                  serverBot mvState
                where
                  bootDeadUsers s = boot (filter dead (users s)) s
                  boot [] s = return s
                  boot (u:us) s = do
                    let (hndl,_) = handle u
                    _ <- hPutStrLn hndl "You are being booted from server for not responding to PING in a timely manner."
                    _ <- hClose hndl
                    boot us $ removeUserFromState u s
                  pingUser :: User -> IO ()
                  pingUser u = do
                            let (h,_) = handle u
                            hPutStrLn h "PING"
                  pingEveryone s = do 
                                  mapM_ pingUser $ users s
                                  return $ s{users=map (\u->u{dead=True}) (users s)}

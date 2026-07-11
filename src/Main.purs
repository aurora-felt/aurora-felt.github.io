module Main where

import Prelude

import Blog as Blog
import Effect (Effect)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Location (host, reload)
import Web.HTML.Window (location)
import Web.Socket.Event.EventTypes (onMessage)
import Web.Socket.WebSocket as WS

main :: Effect Unit
main = do
  reloadHandler
  Blog.main 

reloadHandler :: Effect Unit
reloadHandler = do
  http_url <- window >>= location >>= host 
  let ws_url = "ws://" <> http_url <> "/ws" 
  ws <- WS.create ws_url []
  let reloadPage = const $ window >>= location >>= reload
  messageListener <- eventListener reloadPage 
  addEventListener onMessage messageListener true (WS.toEventTarget ws)


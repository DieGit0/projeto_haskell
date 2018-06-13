{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-} 
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Autenticar where

import Import
import Yesod.Form.Bootstrap3
import Database.Persist.Postgresql
import Yesod.Auth

loginPostForm:: Form (Text,Text)
loginPostForm = renderDivs $ (,)  
                  <$> areq emailField (bfs ("email"::Text)) Nothing
                  <*> areq passwordField (bfs ("senha"::Text)) Nothing
                  
                  
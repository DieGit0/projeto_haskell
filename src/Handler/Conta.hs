{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Conta where

import Import
import Yesod.Form.Bootstrap3
import Database.Persist.Postgresql
import Text.Julius
import Text.Lucius

contaPostForm:: Form User
contaPostForm = renderDivs $ User
                  <$> areq textField (bfs ("nome"::Text)) Nothing
                  <*> areq emailField (bfs ("email"::Text)) Nothing
                  <*> areq passwordField (bfs ("senha"::Text)) Nothing
                  <*> areq dayField (bfs ("aniversário"::Text)) Nothing

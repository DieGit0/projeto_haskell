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

getContaR:: Handler Html
getContaR = do
    session <- lookupSession "_NOME"
    case session of
          Nothing -> do
              redirect HomeR
          Just ("admin")  ->  do
              defaultLayout $ do
                 toWidget $(juliusFile "templates/conta.julius")
                 toWidget $(luciusFile "templates/conta.lucius")
                 $(whamletFile "templates/conta.hamlet")
          _ -> do
              (widget,enctype) <- generateFormPost $ contaPostForm
              id <- lookupSession "_ID"
              defaultLayout $ do
                 toWidget $(juliusFile "templates/conta_user.julius")
                 toWidget $(luciusFile "templates/conta_user.lucius")
                 $(whamletFile "templates/conta_user.hamlet")
     

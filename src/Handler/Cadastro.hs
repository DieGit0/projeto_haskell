{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-} 
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Cadastro where

import Import
import Yesod.Form.Bootstrap3
import Database.Persist.Postgresql

cadastroPostForm:: Form User
cadastroPostForm = renderDivs $ User  
                  <$> areq textField (bfs ("nome"::Text)) Nothing
                  <*> areq emailField (bfs ("email"::Text)) Nothing 
                  <*> areq passwordField (bfs ("senha"::Text)) Nothing 
                  <*> areq dayField (bfs ("aniversário"::Text)) Nothing

getCadastrarR:: Handler Html
getCadastrarR = do
     (widget,enctype) <- generateFormPost $ cadastroPostForm
     defaultLayout $ do
        $(widgetFile "cadastrar")

postCadastrarR:: Handler Html
postCadastrarR = do
    ((res,_),_) <- runFormPost cadastroPostForm
    case res of
        FormSuccess user -> do
            uid <- runDB $ insert user
            defaultLayout $ do 
               [whamlet|
                    <h1>
                        User #{fromSqlKey uid} inserido(a) com sucesso!  
               |]
        _ -> redirect HomeR        
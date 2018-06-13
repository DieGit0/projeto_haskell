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
import Text.Julius
import Text.Lucius

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
               toWidget
                    [julius|
                        swal({
                              title: "Cadastro Realizado!",
                              text: "Usuário inserido com sucesso.",
                              type: 'success',
                              confirmButtonText: 'OK'
                            }).then((result) => {
                                window.location.replace("/");
                            })
                    |]                 
        _ -> redirect HomeR        
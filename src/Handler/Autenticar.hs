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
                  
getAutenticarR:: Handler Html
getAutenticarR = do
        session <- lookupSession "_NOME"
        case session of
            Nothing -> do
        	    (widget,enctype) <- generateFormPost $ loginPostForm
                    defaultLayout $ do
                        $(widgetFile "login")
            _ -> do
                redirect HomeR

autentica :: Text -> Text -> HandlerT App IO (Maybe (Entity User))
autentica email senha = runDB $ selectFirst [UserEmail ==. email
                                            ,UserSenha ==. senha] []

postAutenticarR:: Handler Html
postAutenticarR = do 
    ((resultado,_),_) <- runFormPost $ loginPostForm
    case resultado of
        FormSuccess ("root@root.com","root") -> do        
            setSession "_NOME" "admin"
            setSession "_ID"   "0"
            redirect ContaR
        FormSuccess (email,senha) -> do 
            talvezUsuario <- autentica email senha
            case talvezUsuario of 
                Nothing -> do 
                    --setMessage [shamlet|<div>Usuario nao encontrado/Senha invalida!|]
                    redirect AutenticarR
                Just (Entity id cli) -> do 
                    setSession "_NOME" (userNome cli)
                    setSession "_ID"   (fromString (show (fromSqlKey id))) 
                    redirect HomeR
            redirect AutenticarR
        _ -> redirect HomeR

postSessionR:: Handler Value
postSessionR = do
    session <- lookupSession "_NOME"
    sessionid <- lookupSession "_ID"
    sendStatusJSON ok200 (object ["resp" .= (toJSON session),"id" .= (toJSON sessionid)])

getDeslogarR :: Handler Html
getDeslogarR = do 
    deleteSession "_NOME"
    deleteSession "_ID"
    redirect AutenticarR 
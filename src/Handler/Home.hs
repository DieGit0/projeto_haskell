{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

getHomeR :: Handler Html
getHomeR = do
    session <- lookupSession "_NOME"
    case session of
        Nothing -> do
            redirect AutenticarR
        _   ->   do    
            filmesLista <- runDB $ selectList [] [Asc FilmeNome]
            filmes      <- return $ fmap (\(Entity _ filme) -> filme) filmesLista
            categoriasLista <- runDB $ selectList [] [Asc CategoriaNome]
            diretoresLista <- runDB $ selectList [DiretorId !=. (toSqlKey 16)] [Asc DiretorNome]  
            defaultLayout $ do
                $(widgetFile "homepage")

postHomeR :: Handler Value
postHomeR = do
    nome <- runInputPost $ ireq textField "nome"
    case nome of
        " " -> do
            filmesLista <- runDB $ selectList [] [Asc FilmeNome]
            filmes      <- return $ fmap (\(Entity _ filme) -> filme) filmesLista
            sendStatusJSON ok200 (object ["resp" .= (toJSON filmes)])
        _ -> do
            filmesLista <- runDB $ selectList [Filter FilmeNome (Left $ "%"++nome++"%") (BackendSpecificFilter "LIKE")] [Asc FilmeNome]
            filmes      <- return $ fmap (\(Entity _ filme) -> filme) filmesLista
            sendStatusJSON ok200 (object ["resp" .= (toJSON filmes)])

postDiretorR :: Handler Value
postDiretorR  = do
    diretorid   <- runInputPost $ ireq intField "diretorId"
    filmesLista <- runDB $ selectList [FilmeDid ==. (toSqlKey diretorid)] [Asc FilmeNome]
    filmes      <- return $ fmap (\(Entity _ filme) -> filme) filmesLista
    sendStatusJSON ok200 (object ["resp" .= (toJSON filmes)])
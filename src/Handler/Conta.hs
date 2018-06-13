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

postListaFilmes:: Handler Value
postListaFilmes = do
      filmesLista <- runDB $ selectList [] [Asc FilmeNome]
      filmes      <- return $ fmap (\(Entity _ filme) -> filme) filmesLista
      filmesdid       <- return $ fmap (\(Filme _ _ _ filmedid) -> filmedid) filmes
      diretoresLista    <- sequence $ fmap (\did -> runDB $ selectList [DiretorId ==. did] []) filmesdid
      sendStatusJSON ok200 (object ["resp" .= (toJSON filmesLista),"dir" .= (toJSON diretoresLista)])

postListaDiretores:: Handler Value
postListaDiretores = do
      diretoresLista <- runDB $ selectList [] [Asc DiretorNome]
      sendStatusJSON ok200 (object ["resp" .= (toJSON diretoresLista)])

postListaCategorias:: Handler Value
postListaCategorias = do
     categoriasLista <- runDB $ selectList [] [Asc CategoriaNome]
     sendStatusJSON ok200 (object ["resp" .= (toJSON categoriasLista)])

postAddDiretores:: Text -> Handler Value
postAddDiretores nome = do
     did <- runDB $ insert (Diretor nome)
     sendStatusJSON ok200 (object ["resp" .= (toJSON did)])

postDelDiretores:: DiretorId -> Handler Value
postDelDiretores did = do
     runDB $ updateWhere [FilmeDid ==. did] [FilmeDid =. (toSqlKey 16)]
     runDB $ delete did
     sendStatusJSON ok200 (object ["resp" .= (toJSON True)])

postAddCategorias:: Text -> Handler Value
postAddCategorias nome = do
      cid <- runDB $ insert (Categoria nome)
      sendStatusJSON ok200 (object ["resp" .= (toJSON cid)])

postDelCategorias:: CategoriaId -> Handler Value
postDelCategorias cid = do
      runDB $ deleteWhere [CategoriaFilmeCid ==. cid]
      runDB $ delete cid
      sendStatusJSON ok200 (object ["resp" .= (toJSON True)])

postDelFilmes:: FilmeId -> Handler Value
postDelFilmes id = do
      runDB $ deleteWhere [CategoriaFilmeFid ==. id]
      runDB $ delete id
      sendStatusJSON ok200 (object ["resp" .= (toJSON True)])

postCarregarUser:: UserId -> Handler Value
postCarregarUser uid = do
     retorno <- runDB $ get404 uid
     sendStatusJSON ok200 (object ["resp" .= (toJSON retorno)])

postAtualizarUser:: UserId -> Text -> Text -> Text -> Day -> Handler Value
postAtualizarUser uid nome email senha dataNasc = do
     runDB $ updateWhere [UserId ==. uid] [UserNome =. nome, UserEmail =. email, UserSenha =. senha, UserDataNasc =. dataNasc]
     sendStatusJSON ok200 (object ["resp" .= (toJSON True)])         

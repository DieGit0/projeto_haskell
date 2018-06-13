{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Admin where

import Import
import Yesod.Form.Bootstrap3

filmePostForm:: Form (Text, FileInfo, FileInfo, Key Diretor, Key Categoria)
filmePostForm = renderDivs $ (,,,,)  
                  <$> areq textField (bfs ("NOME: "::Text)) Nothing                  
                  <*> areq fileField FieldSettings{fsId=Just "hident3",fsLabel="FOTO: ", fsTooltip= Nothing,fsName= Nothing,fsAttrs=[("accept",".png,.jpg,.jpeg")]} Nothing
                  <*> areq fileField FieldSettings{fsId=Just "hident4",fsLabel="ARQUIVO: ", fsTooltip= Nothing,fsName= Nothing,fsAttrs=[("accept",".webm,.ogg,.mp4")]} Nothing
                  <*> areq (selectField catchDiretores) (bfs ("DIRETOR: "::Text)) Nothing
                  <*> areq (selectField catchCategorias) (bfs ("CATEGORIA: "::Text)) Nothing

catchDiretores = do
       entidades <- runDB $ selectList [] [Asc DiretorNome] 
       optionsPairs $ fmap (\ent -> (diretorNome $ entityVal ent, entityKey ent)) entidades

catchCategorias = do
       entidades <- runDB $ selectList [] [Asc CategoriaNome] 
       optionsPairs $ fmap (\ent -> (categoriaNome $ entityVal ent, entityKey ent)) entidades                         

postAdminR:: Handler Html
postAdminR = do
    ((res,_),_) <- runFormPost filmePostForm
    case res of
        FormSuccess (nome, foto, arquivo,diretor,categoria) -> do            
            liftIO $ fileMove foto ("static/img/" ++ (unpack $ fileName foto))
            liftIO $ fileMove arquivo ("static/filmes/" ++ (unpack $ fileName arquivo))
            fid <- runDB $ insert (Filme nome (fileName foto) (fileName arquivo) diretor)
            _ <- runDB $ insert (CategoriaFilme categoria fid)
            redirect HomeR             
    redirect HomeR

getAdminR:: Handler Html
getAdminR = do
        (widget,enctype) <- generateFormPost $ filmePostForm
        defaultLayout $ do
                sessao <- lookupSession "_NOME"
                case sessao of
                    Just ("admin") -> do
                        $(widgetFile "admin")                      
                    _ -> redirect HomeR
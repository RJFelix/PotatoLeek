{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.Property where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

getPropertyR :: Handler Html
getPropertyR = do
    allEnterprises <- runDB $ selectList [] [Asc EnterpriseId]
    mTextId <- lookupGetParam "enterpriseId"
    case mTextId of
        Nothing -> do allProperties <- getProperties Nothing
                      renderGetPropertyR allEnterprises allProperties Nothing
        Just textId -> do let eid = parseToKey textId
                          ment <- getFromDBWithMaybeId eid
                          allProperties <- getProperties $ const <$> eid <*> ment
                          renderGetPropertyR allEnterprises allProperties $ (,) <$> eid <*> ment

getFromDBWithMaybeId :: Maybe EnterpriseId -> Handler (Maybe Enterprise)
getFromDBWithMaybeId Nothing    = pure Nothing
getFromDBWithMaybeId (Just eid) = runDB $ get eid
    
getProperties :: Maybe EnterpriseId -> Handler [Entity Property]
getProperties eid = runDB $ selectList (((==.) PropertyEnterpriseId) <$> (maybeToList eid)) [Asc PropertyId]
                                        
renderGetPropertyR :: [Entity Enterprise] -> [Entity Property] -> Maybe (EnterpriseId, Enterprise) -> Handler Html
renderGetPropertyR allEnterprises allProperties selectedE = do
    (addPropertyFormWidget, enctype) <- generateFormPost (addPropertyForm (fst <$> selectedE) allEnterprises)
    defaultLayout $ do
        let enterpriseName = (enterpriseTitle . snd) <$> selectedE
        setTitle . toHtml $ maybe "All Properties" (\t -> t <> " - Properties") enterpriseName
        $(widgetFile "property-list")
    
postPropertyR :: Handler Html
postPropertyR = do
    allEnterprises <- runDB $ selectList [] [Asc EnterpriseId]
    ((result, addPropertyFormWidget), enctype) <- runFormPost (addPropertyForm Nothing allEnterprises)
    _ <- case result of
        FormSuccess newProperty -> do
            runDB $ insert $ newProperty
    allProperties <- runDB $ selectList [] [Asc PropertyId]
    defaultLayout $ do
        let enterpriseName = (Nothing :: Maybe Text)
        setTitle "All Properties"
        $(widgetFile "property-list")

getPropertyIdR :: PropertyId -> Handler Html
getPropertyIdR pid = do
    p <- runDB $ get404 pid
    defaultLayout $ do
        let pTitle = propertyTitle p
            pDescription = propertyDescription p
        setTitle . toHtml $ pTitle
        $(widgetFile "property-page")


addPropertyForm :: Maybe EnterpriseId -> [Entity Enterprise] -> Form Property
addPropertyForm maybeE es = renderBootstrap3 BootstrapBasicForm $ Property
    <$> areq textField "Name" Nothing
    <*> areq textField "Description" Nothing
    <*> areq (selectFieldList ents) "Enterprise" maybeE
  where
    ents :: [(Text, EnterpriseId)]
    ents = fmap idAndName es
      where idAndName (Entity eid ent) = (enterpriseTitle ent, eid)
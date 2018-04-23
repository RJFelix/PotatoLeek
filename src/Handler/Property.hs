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
    es <- runDB $ selectList [] [Asc EnterpriseId]
    (updatePropertyFormWidget, enctype) <- generateFormPost (updatePropertyForm es (Just p))
    defaultLayout $ do
        let pTitle = propertyTitle p
            pDescription = propertyDescription p
        setTitle . toHtml $ pTitle
        $(widgetFile "property-page")

putPropertyIdR :: PropertyId -> Handler Html
putPropertyIdR pid = do
    es <- runDB $ selectList [] [Asc EnterpriseId]
    ((result, _), _) <- runFormPost (updatePropertyForm es Nothing)
    _ <- case result of
        FormSuccess updatedProperty -> do
            runDB $ repsert pid updatedProperty
    p <- runDB $ get404 pid
    (updatePropertyFormWidget, enctype) <- generateFormPost (updatePropertyForm es (Just p))
    defaultLayout $ do
        let pTitle = propertyTitle p
            pDescription = propertyDescription p
        setTitle . toHtml $ pTitle
        $(widgetFile "property-page")

deletePropertyIdR :: PropertyId -> Handler ()
deletePropertyIdR pid = (runDB $ delete pid) >> redirect PropertyR

addPropertyForm :: Maybe EnterpriseId -> [Entity Enterprise] -> Form Property
addPropertyForm maybeE es = renderBootstrap3 BootstrapBasicForm $ Property
    <$> areq textField "Name" Nothing
    <*> areq textField "Description" Nothing
    <*> areq (selectFieldList $ toFormListOn enterpriseTitle es) "Enterprise" maybeE

updatePropertyForm :: [Entity Enterprise] -> Maybe Property -> Form Property
updatePropertyForm es p = renderBootstrap3 BootstrapBasicForm $ Property
    <$> areq textField "Name" (propertyTitle <$> p)
    <*> areq textField "Description" (propertyDescription <$> p)
    <*> areq (selectFieldList $ toFormListOn enterpriseTitle es) "Enterprise" (propertyEnterpriseId <$> p)
--   where
--     ents :: [(Text, EnterpriseId)]
--     ents = fmap idAndName es
--         where idAndName (Entity eid ent) = (enterpriseTitle ent, eid)

toFormListOn :: (a -> Text) -> [Entity a] -> [(Text, Key a)]
toFormListOn f es = fmap idAndText es
    where idAndText (Entity eid ent) = (f ent, eid)
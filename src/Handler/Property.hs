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
        
postPropertyR :: Handler Html
postPropertyR = do
    allEnterprises <- runDB $ selectList [] [Asc EnterpriseId]
    ((result, _), _) <- runFormPost (propertyForm allEnterprises Nothing Nothing)
    case result of
        FormSuccess newProperty -> do
            pid <- runDB $ insert $ newProperty
            redirect (PropertyIdR pid)
        (FormFailure _) -> do
            allProperties <- runDB $ selectList [] [Asc PropertyId]
            renderGetPropertyR allEnterprises allProperties Nothing
        FormMissing -> do
            allProperties <- runDB $ selectList [] [Asc PropertyId]
            renderGetPropertyR allEnterprises allProperties Nothing

getFromDBWithMaybeId :: Maybe EnterpriseId -> Handler (Maybe Enterprise)
getFromDBWithMaybeId Nothing    = pure Nothing
getFromDBWithMaybeId (Just eid) = runDB $ get eid
    
getProperties :: Maybe EnterpriseId -> Handler [Entity Property]
getProperties eid = runDB $ selectList (((==.) PropertyEnterpriseId) <$> (maybeToList eid)) [Asc PropertyId]
                                        
renderGetPropertyR :: [Entity Enterprise] -> [Entity Property] -> Maybe (EnterpriseId, Enterprise) -> Handler Html
renderGetPropertyR allEnterprises allProperties selectedE = do
    (addPropertyFormWidget, enctype) <- generateFormPost (propertyForm allEnterprises (fst <$> selectedE) Nothing)
    defaultLayout $ do
        let enterpriseName = (enterpriseTitle . snd) <$> selectedE
        setTitle . toHtml $ maybe "All Properties" (\t -> t <> " - Properties") enterpriseName
        $(widgetFile "property-list")

getPropertyIdR :: PropertyId -> Handler Html
getPropertyIdR pid = do
    p <- runDB $ get404 pid
    es <- runDB $ selectList [] [Asc EnterpriseId]
    renderPropertyIdR es (pid, p)

putPropertyIdR :: PropertyId -> Handler Html
putPropertyIdR pid = do
    es <- runDB $ selectList [] [Asc EnterpriseId]
    ((result, _), _) <- runFormPost (propertyForm es Nothing Nothing)
    case result of
        FormSuccess updatedProperty -> do
            runDB $ repsert pid updatedProperty
            p <- runDB $ get404 pid
            renderPropertyIdR es (pid, p)
        FormMissing -> do
            p <- runDB $ get404 pid
            renderPropertyIdR es (pid, p)
        (FormFailure _) -> do
            p <- runDB $ get404 pid
            renderPropertyIdR es (pid, p)

deletePropertyIdR :: PropertyId -> Handler ()
deletePropertyIdR pid = (runDB $ delete pid) >> redirect PropertyR

renderPropertyIdR :: [Entity Enterprise] -> (PropertyId, Property) -> Handler Html
renderPropertyIdR es (pid, p) = do
    (updatePropertyFormWidget, enctype) <- generateFormPost (propertyForm es Nothing (Just p))
    defaultLayout $ do
        let pTitle = propertyTitle p
            pDescription = propertyDescription p
        setTitle . toHtml $ pTitle
        $(widgetFile "property-page")

propertyForm :: [Entity Enterprise] -> Maybe EnterpriseId -> Maybe Property -> Form Property
propertyForm es mEntId mProp = renderBootstrap3 BootstrapBasicForm $ Property
    <$> areq textField "Name" (propertyTitle <$> mProp)
    <*> areq textField "Description" (propertyDescription <$> mProp)
    <*> areq (selectFieldList $ toFormListOn enterpriseTitle es) "Enterprise" (maybe mEntId (Just . propertyEnterpriseId) mProp)
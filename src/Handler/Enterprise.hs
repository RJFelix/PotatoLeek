{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Enterprise where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
-- import Model

getEnterpriseR :: Handler Html
getEnterpriseR = do
    (addEnterpriseFormWidget, enctype) <- generateFormPost addEnterpriseForm
    allEnterprises <- runDB $ selectList [] [Asc EnterpriseId]
    defaultLayout $ do
        setTitle "All Enterprises"
        $(widgetFile "enterprise-list")

postEnterpriseR :: Handler Html
postEnterpriseR = do
    ((result, addEnterpriseFormWidget), enctype) <- runFormPost addEnterpriseForm
    _ <- case result of
        FormSuccess newEnterprise -> do
            runDB $ insert $ newEnterprise
    allEnterprises <- runDB $ selectList [] [Asc EnterpriseId]
    defaultLayout $ do
        setTitle "All Enterprises"
        $(widgetFile "enterprise-list")

getEnterpriseIdR :: EnterpriseId -> Handler Html
getEnterpriseIdR eid = do
    e <- runDB $ get404 eid
    (updateEnterpriseFormWidget, enctype) <- generateFormPost (updateEnterpriseForm (Just e))
    defaultLayout $ do
        let eTitle = enterpriseTitle e
            eDescription = enterpriseDescription e
        setTitle . toHtml $ eTitle
        $(widgetFile "enterprise-page")

putEnterpriseIdR :: EnterpriseId -> Handler Html
putEnterpriseIdR eid = do
    ((result, _), _) <- runFormPost (updateEnterpriseForm Nothing)
    _ <- case result of
        FormSuccess updatedEnterprise -> do
            runDB $ repsert eid updatedEnterprise
    e <- runDB $ get404 eid
    (updateEnterpriseFormWidget, enctype) <- generateFormPost (updateEnterpriseForm (Just e))
    defaultLayout $ do
        let eTitle = enterpriseTitle e
            eDescription = enterpriseDescription e
        setTitle . toHtml $ eTitle
        $(widgetFile "enterprise-page")

deleteEnterpriseIdR :: EnterpriseId -> Handler ()
deleteEnterpriseIdR eid = do
    runDB $ delete eid
    redirect EnterpriseR

addEnterpriseForm :: Form Enterprise
addEnterpriseForm = renderBootstrap3 BootstrapBasicForm $ Enterprise
    <$> areq textField "Name" Nothing
    <*> areq textField "Description" Nothing

updateEnterpriseForm :: Maybe Enterprise -> Form Enterprise
updateEnterpriseForm e = renderBootstrap3 BootstrapBasicForm $ Enterprise
    <$> areq textField "Name" (enterpriseTitle <$> e)
    <*> areq textField "Description" (enterpriseDescription <$> e)
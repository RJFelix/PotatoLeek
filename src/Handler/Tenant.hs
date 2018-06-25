{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.Tenant where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

getTenantR :: Handler Html
getTenantR = do
    (addTenantFormWidget, enctype) <- generateFormPost addTenantForm
    allTenants <- runDB $ selectList [] [Asc TenantId]
    defaultLayout $ do
        setTitle "All Tenants"
        $(widgetFile "tenant-list")

postTenantR :: Handler Html
postTenantR = do
    ((result, addTenantFormWidget), enctype) <- runFormPost addTenantForm
    _ <- case result of
        FormSuccess newTenant -> do
            runDB $ insert $ newTenant
    allTenants <- runDB $ selectList [] [Asc TenantId]
    defaultLayout $ do
        setTitle "All Tenants"
        $(widgetFile "tenant-list")

getTenantIdR :: TenantId -> Handler Html
getTenantIdR tid = do
    t <- runDB $ get404 tid
    (updateTenantFormWidget, enctype) <- generateFormPost (updateTenantForm (Just t))
    defaultLayout $ do
        let tName = tenantName t
        setTitle . toHtml $ tName
        $(widgetFile "tenant-page")

putTenantIdR :: TenantId -> Handler Html
putTenantIdR tid = do
    ((result, _), _) <- runFormPost (updateTenantForm Nothing)
    _ <- case result of
        FormSuccess updatedTenant -> do
            runDB $ repsert tid updatedTenant
    t <- runDB $ get404 tid
    (updateTenantFormWidget, enctype) <- generateFormPost (updateTenantForm (Just t))
    defaultLayout $ do
        let tName = tenantName t
        setTitle . toHtml $ tName
        $(widgetFile "tenant-page")

deleteTenantIdR :: TenantId -> Handler ()
deleteTenantIdR tid = do
    runDB $ delete tid
    redirect TenantR

addTenantForm :: Form Tenant
addTenantForm = renderBootstrap3 BootstrapBasicForm $ Tenant
    <$> areq textField "Name" Nothing
    <*> areq textField "Phone Number" Nothing
    <*> areq textField "Email Address" Nothing
    <*> areq textField "Notes" Nothing

updateTenantForm :: Maybe Tenant -> Form Tenant
updateTenantForm t = renderBootstrap3 BootstrapBasicForm $ Tenant
    <$> areq textField "Name" (tenantName <$> t)
    <*> areq textField "Phone Number" (tenantPhone <$> t)
    <*> areq textField "Email Address" (tenantEmail <$> t)
    <*> areq textField "Notes" (tenantNotes <$> t)
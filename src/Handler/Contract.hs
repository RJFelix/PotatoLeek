{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.Contract where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Data.Time.Calendar

getContractR :: Handler Html
getContractR = do
    allUnits <- runDB $ selectList [] [Asc UnitId]
    allTenants <- runDB $ selectList [] [Asc TenantId]
    allContracts <- runDB $ selectList [] [Asc ContractId]
    renderGetContractR allUnits allTenants allContracts

postContractR :: Handler Html
postContractR = do
    allUnits <- runDB $ selectList [] [Asc UnitId]
    allTenants <- runDB $ selectList [] [Asc TenantId]
    ((result, _), _) <- runFormPost (contractForm allUnits allTenants Nothing)
    _ <- case result of
        FormSuccess newContract -> do
            runDB $ insert newContract
    allContracts <- runDB $ selectList [] [Asc ContractId]
    renderGetContractR allUnits allTenants allContracts

renderGetContractR :: [Entity Unit] -> [Entity Tenant] -> [Entity Contract] -> Handler Html
renderGetContractR allUnits allTenants allContracts = do
    (addContractFormWidget, enctype) <- generateFormPost (contractForm allUnits allTenants Nothing)
    defaultLayout $ do
        setTitle "All Contracts"
        $(widgetFile "contract-list")

getContractIdR :: ContractId -> Handler Html
getContractIdR cid = do
    contract <- runDB $ get404 cid
    allUnits <- runDB $ selectList [] [Asc UnitId]
    allTenants <- runDB $ selectList [] [Asc TenantId]
    renderGetContractIdR allUnits allTenants (cid, contract)

putContractIdR :: ContractId -> Handler Html
putContractIdR cid = do
    allUnits <- runDB $ selectList [] [Asc UnitId]
    allTenants <- runDB $ selectList [] [Asc TenantId]
    ((result, _), _) <- runFormPost (contractForm allUnits allTenants Nothing)
    _ <- case result of
        FormSuccess updatedContract -> do
            runDB $ repsert cid updatedContract
    c <- runDB $ get404 cid
    renderGetContractIdR allUnits allTenants (cid, c)

deleteContractIdR :: ContractId -> Handler Html
deleteContractIdR cid = do
    runDB $ delete cid 
    redirect ContractR

renderGetContractIdR :: [Entity Unit] -> [Entity Tenant] -> (ContractId, Contract) -> Handler Html
renderGetContractIdR allUnits allTenants (cid, contract) = do
    (updateContractFormWidget, enctype) <- generateFormPost (contractForm allUnits allTenants (Just contract))
    defaultLayout $ do
        setTitle "Contract View"
        $(widgetFile "contract-page")

-- TODO: Probably replace this with a monadic MForm or WForm
contractForm :: [Entity Unit] -> [Entity Tenant] -> Maybe Contract -> Form Contract
contractForm us ts c = renderBootstrap3 BootstrapBasicForm $ Contract
    <$> areq (selectFieldList $ toFormListOn unitTitle us) "Unit" (contractUnitId <$> c)
    <*> areq (selectFieldList $ toFormListOn tenantName ts) "Tenant" (contractTenantId <$> c)
    <*> areq intField "Rent" (contractRent <$> c)
    <*> areq dayField "Start Date" (contractStartDate <$> c)
    <*> areq dayField "End Date" (contractEndDate <$> c)
    <*> areq boolField "Active" (contractActive <$> c)
    <*> areq boolField "Open Ended" (contractOpenEnded   <$> c)
    <*> areq intField "Deposit" (contractDeposit <$> c)
    <*> areq (selectField optionsEnum) "Collection Period" (contractCollectionPeriod <$> c)
    <*> areq intField "Collection Frequency" (contractCollectionFrequency <$> c)
    <*> areq dayField "First Collection Day" (contractFirstCollectionDay <$> c)
    <*> areq textField "Notes" (contractNotes <$> c)
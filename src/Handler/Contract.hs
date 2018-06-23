{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.Contract where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

data ContractWithTenant = ContractWithTenant 
    { contract :: Contract
    , tenantId :: TenantId
    }

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
    ((result, _), _) <- runFormPost (contractForm allUnits Nothing allTenants Nothing Nothing)
    _ <- case result of
        FormSuccess newContractWithTenant -> do
            cid <- runDB $ insert $ contract newContractWithTenant
            t <- runDB $ get404 $ tenantId newContractWithTenant
            runDB $ update (tenantId newContractWithTenant) [TenantContracts =. (cid : (tenantContracts t))]
    allContracts <- runDB $ selectList [] [Asc ContractId]
    renderGetContractR allUnits allTenants allContracts

renderGetContractR :: [Entity Unit] -> [Entity Tenant] -> [Entity Contract] -> Handler Html
renderGetContractR allUnits allTenants allContracts = do
    (addContractFormWidget, enctype) <- generateFormPost (contractForm allUnits Nothing allTenants Nothing Nothing)
    defaultLayout $ do
        setTitle "All Contracts"
        $(widgetFile "contract-list")

getContractIdR :: ContractId -> Handler Html
getContractIdR cid = redirect ContractR

putContractIdR :: ContractId -> Handler Html
putContractIdR cid = redirect ContractR

deleteContractIdR :: ContractId -> Handler Html
deleteContractIdR cid = runDB $ delete cid >> redirect ContractR

renderGetContractIdR :: [Entity Unit] -> [Entity Tenant] -> (ContractId, Contract) -> Handler Html
renderGetContractIdR allUnits allTenants (contractId, contract) = redirect ContractR

-- TODO: Probably replace this with a monadic MForm or WForm
contractForm :: [Entity Unit] -> Maybe UnitId -> [Entity Tenant] -> Maybe TenantId -> Maybe ContractWithTenant -> Form ContractWithTenant
contractForm us uid ts tid c = renderBootstrap3 BootstrapBasicForm $ 
    (\unid r sd ed a oe d cp cf fcd n teid -> ContractWithTenant (Contract unid r sd ed a oe d cp cf fcd n) teid)
    <$> areq (selectFieldList $ toFormListOn unitTitle us) "Unit" (maybe uid (Just . contractUnitId . contract) c)
    <*> areq intField "Rent" (contractRent . contract <$> c)
    <*> areq dayField "Start Date" (contractStartDate . contract <$> c)
    <*> areq dayField "End Date" (contractEndDate . contract <$> c)
    <*> areq boolField "Active" (contractActive . contract <$> c)
    <*> areq boolField "Open Ended" (contractOpenEnded . contract <$> c)
    <*> areq intField "Deposit" (contractDeposit . contract <$> c)
    <*> areq (selectField optionsEnum) "Collection Period" (contractCollectionPeriod . contract <$> c)
    <*> areq intField "Collection Frequency" (contractCollectionFrequency . contract <$> c)
    <*> areq dayField "First Collection Day" (contractFirstCollectionDay . contract <$> c)
    <*> areq textField "Notes" (contractNotes . contract <$> c)
    <*> areq (selectFieldList $ toFormListOn tenantName ts) "Tenant" (maybe tid (Just . tenantId) c)
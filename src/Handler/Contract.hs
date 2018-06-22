{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.Contract where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

getContractR :: Handler Html
getContractR = do
    allTenants <- runDB $ selectList [] [Asc TenantId]
    allContracts <- runDB $ selectList [] [Asc ContractId]
    renderGetContractR allContracts

postContractR :: Handler Html
postContractR = do
    allTenants <- runDB $ selectList [] [Asc TenantId] -- necessary?
    ((result, _), _) <- runFormPost (contractForm Nothing)
    _ <- case result of
        FormSuccess newContract -> runDB $ insert $ newContract
        -- todo: how to properly add this contract's id to tenant's contract list?
    allContracts <- runDB $

renderGetContractR :: [Entity Tenant] -> [Entity Contract] -> Handler Html

getContractIdR :: ContractId -> Handler Html

putContractIdR :: ContractId -> Handler Html

deleteContractIdR :: ContractId -> Handler Html
deleteContractIdR cid = runDB $ delete cid >> redirect ContractR

renderGetContractIdR :: [Entity Tenant] -> (ContractId, Contract) -> Handler Html

-- TODO: Contract likely replaced with a Contract + TenantId type
contractForm :: [Entity Tenant] -> Maybe TenantId -> Maybe Contract -> Form Contract

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.Unit where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

getUnitR :: Handler Html
getUnitR = do
    allUnits <- runDB $ selectList [] [Asc UnitId]
    allProperties <- runDB $ selectList [] [Asc PropertyId]
    renderGetUnitR allProperties allUnits

postUnitR :: Handler Html
postUnitR = do
    allProperties <- runDB $ selectList [] [Asc PropertyId]
    ((result, _), _) <- runFormPost (unitForm allProperties Nothing Nothing)
    _ <- case result of
        FormSuccess newUnit -> runDB $ insert $ newUnit
    allUnits <- runDB $ selectList [] [Asc UnitId]
    renderGetUnitR allProperties allUnits

renderGetUnitR :: [Entity Property] -> [Entity Unit] -> Handler Html
renderGetUnitR allProperties allUnits = do
    (addUnitFormWidget, enctype) <- generateFormPost (unitForm allProperties Nothing Nothing)
    defaultLayout $ do
        setTitle "All Units"
        $(widgetFile "unit-list")

getUnitIdR :: UnitId -> Handler Html
getUnitIdR unitId = do
    unit <- runDB $ get404 unitId
    ps <- runDB $ selectList [] [Asc PropertyId]
    renderGetUnitIdR ps (unitId, unit)

-- TODO: make this work
putUnitIdR :: UnitId -> Handler Html
putUnitIdR unitId = do
    unit <- runDB $ get404 unitId
    ps <- runDB $ selectList [] [Asc PropertyId]
    renderGetUnitIdR ps (unitId, unit)

deleteUnitIdR :: UnitId -> Handler ()
deleteUnitIdR unitId = runDB $ delete uid >> redirect UnitR

renderGetUnitIdR :: [Entity Property] -> (UnitId, Unit) -> Handler Html
renderGetUnitIdR allProperties (unitId, unit) = do
    (updateUnitFormWidget, enctype) <- generateFormPost (unitForm allProperties Nothing (Just unit))
    defaultLayout $ do
        setTitle . toHtml $ unitTitle unit
        $(widgetFile "unit-page")

unitForm :: [Entity Property] -> Maybe PropertyId -> Maybe Unit -> Form Unit
unitForm ps pid u = renderBootstrap3 BootstrapBasicForm $ Unit
    <$> areq textField "Name"        (unitTitle <$> u)
    <*> areq textField "Description" (unitDescription <$> u)
    <*> areq intField  "Capacity"    (unitCapacity <$> u)
    <*> areq (selectFieldList $ toFormListOn propertyTitle ps) "Property" (maybe pid (Just . unitPropertyId) u)
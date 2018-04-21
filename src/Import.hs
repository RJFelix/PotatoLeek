{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Import
    ( module Import
    ) where

import Foundation            as Import
import Import.NoFoundation   as Import
import Import.NoFoundation ((.))
import Database.Persist.Sql (ToBackendKey, SqlBackend, toSqlKey)
import Data.Text.Read (Reader, decimal)

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right a) = Just a

type ParsedKey = Data.Text.Read.Reader Int64
parseToKey :: ToBackendKey SqlBackend e => Text -> Maybe (Key e)
parseToKey textId = eitherToMaybe ((fmap (toSqlKey . fst) . (decimal :: ParsedKey)) textId)
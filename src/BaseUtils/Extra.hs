{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : BaseUtils.Extra
Description : miscellaneous functions
Copyright   : (c) Grant Weyburne, 2022
License     : BSD-3
-}
module BaseUtils.Extra (
  rethrow,
  innerMostGBWrapException,
  isWindows,
  pathSeparator,
  hoistEitherM,
  hoistValidationM,
  --  GBException' (..),
  --  GBWrapException' (..),
  pattern GBException,
  pattern GBWrapException,
) where

import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Typeable
import qualified GHC.Generics as G (Generic)
import GHC.Stack
import qualified System.Info as SI
import Text.Pretty.Simple
import qualified UnliftIO as U
import qualified Validation as V

-- | rethrow a wrapped exception unless already wrapped
rethrow :: (MonadIO m, HasCallStack, U.Exception e) => Text -> e -> m a
rethrow txt e =
  if typeOf e `elem` [typeRep (Proxy @GBException'), typeRep (Proxy @GBWrapException')]
    then U.throwIO e
    else U.throwIO $ GBWrapException txt e

-- | extract innermost unwrapped exception
innerMostGBWrapException :: HasCallStack => U.SomeException -> (Bool, U.SomeException)
innerMostGBWrapException e =
  case U.fromException @GBWrapException' e of
    Just GBWrapException'{gbwException = x} ->
      let (_b, e') = innerMostGBWrapException (U.toException x)
       in (True, e')
    Nothing -> (False, e)

-- | create a custom wrapped exception
{-# COMPLETE GBWrapException #-}

pattern GBWrapException ::
  () =>
  forall e.
  (HasCallStack, U.Exception e) =>
  Text ->
  e ->
  GBWrapException'
pattern GBWrapException msg e <-
  GBWrapException' msg e _
  where
    GBWrapException msg e = GBWrapException' msg e (T.pack (prettyCallStack callStack))

-- | custom wrapped exception
data GBWrapException' = forall e. U.Exception e => GBWrapException' {gbwText :: !Text, gbwException :: !e, gbwCallStack :: !Text}

deriving stock instance Show GBWrapException'
instance U.Exception GBWrapException'

-- | create a custom exception
{-# COMPLETE GBException #-}

pattern GBException ::
  () =>
  HasCallStack =>
  Text ->
  GBException'
pattern GBException txt <-
  GBException' txt _
  where
    GBException txt = GBException' ("{{Logging}}:" <> txt) (T.pack (prettyCallStack callStack))

-- | create a custom exception
data GBException' = GBException' {gbMessage :: !Text, gbCallStack :: !Text}
  deriving stock (G.Generic, Show, Eq)

instance U.Exception GBException'

-- | windows flag
isWindows :: Bool
isWindows = SI.os == "mingw32"

-- | path separator for windows vs unix
pathSeparator :: Char
pathSeparator = if isWindows then '\\' else '/'

-- | lift a 'Either' to an exception
hoistEitherM ::
  (HasCallStack, MonadIO m) =>
  Text ->
  Either Text a ->
  m a
hoistEitherM txt =
  either (\txt1 -> U.throwIO $ GBException (txt <> txt1)) return

-- | lift a 'V.Validation' to an exception
{-# INLINE hoistValidationM #-}
hoistValidationM ::
  (HasCallStack, MonadIO m, Show e) =>
  Text ->
  V.Validation e a ->
  m a
hoistValidationM txt = V.validation (U.throwIO . GBException . (txt <>) . TL.toStrict . pShowOpt defaultOutputOptionsNoColor) return

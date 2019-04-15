module Main where

import Prelude

import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse, print)
import Routing.Hash (getHash, matchesWith, setHash)
import Type.Equality (class TypeEquals, from)

import Capability.LogMessages (class LogMessages)
import Capability.Navigate (class Navigate)
import Data.Route (Route, routeCodec)
import Component.Router as Router

data LogLevel = Dev | Prod

type Env = { logLevel :: LogLevel }

newtype AppM a = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

instance logMessagesAppM :: LogMessages AppM where
  logMessage log = do
    env <- ask
    liftEffect case env.logLevel of
      Prod -> pure unit
      _ -> Console.log $ show log

instance navigateAppM :: Navigate AppM where
  navigate =
    liftEffect <<< setHash <<< print routeCodec

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody

  initialHash <- liftEffect $ getHash

  let
    logLevel = Dev

    environment :: Env
    environment = { logLevel }

    rootComponent :: H.Component HH.HTML Router.Query Router.Input Void Aff
    rootComponent = H.hoist (runAppM environment) Router.component

    initialRoute :: Maybe Route
    initialRoute = hush $ parse routeCodec initialHash

  halogenIO <- runUI rootComponent initialRoute body

  void $ liftEffect $ matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) do
      launchAff_ $ halogenIO.query $ H.action $ Router.Navigate new

  pure unit

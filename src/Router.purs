module Router where

import BigPrelude

import Component.Profile as Profile
import Component.Sessions as Sessions
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log, CONSOLE)
import Control.Monad.State.Class (put)
import Data.Functor.Coproduct (Coproduct)
import Data.String (toLower)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component.ChildPath (ChildPath, cp1, cpL, cpR)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Routing (matchesAff)
import Routing.Match (Match)
import Routing.Match.Class (lit, num)

data Input a
  = Goto Routes a

data CRUD
  = Index
  | Show Number

data Routes
  = Profile
  | Sessions CRUD
  | Home

instance showRoutes :: Show Routes where
  show Profile = "Profile"
  show (Sessions Index) = "Sessions"
  show (Sessions (Show n)) = "Sessions > " <> show n
  show Home = "Home"


init :: State
-- init = { currentPage: "Home" }
init = Home

routing :: Match Routes
routing = profile
      <|> sessions
      <|> home
  where
    profile = Profile <$ route "profile"
    home = Home <$ lit ""
    sessions = Sessions <$> (route "sessions" *> parseCRUD)
    route str = lit "" *> lit str
    parseCRUD = Show <$> num <|> pure Index

-- type State =
--   { currentPage :: String
--   }

type State = Routes

type ChildQuery = Coproduct Profile.Input Sessions.Input
type ChildSlot = Either Profile.Slot Sessions.Slot

pathToProfile :: ChildPath Profile.Input ChildQuery Profile.Slot ChildSlot
pathToProfile = cpL

pathToSessions :: ChildPath Sessions.Input ChildQuery Sessions.Slot ChildSlot
pathToSessions = cpR

type QueryP
  = Coproduct Input ChildQuery

ui :: forall m. H.Component HH.HTML Input Unit Void m
ui = H.parentComponent
  { initialState: const init
  , render
  , eval
  , receiver: const Nothing
  }
  where
    render :: State -> H.ParentHTML Input ChildQuery ChildSlot m
    render st =
      HH.div_
        [ HH.h1_ [ HH.text (show st) ]
        , HH.ul_ (map link ["Sessions", "Profile", "Home"])
        , viewPage st
        ]

    link s = HH.li_ [ HH.a [ HP.href ("#/" <> toLower s) ] [ HH.text s ] ]

    viewPage :: Routes -> H.ParentHTML Input ChildQuery ChildSlot m
    viewPage (Sessions Index) =
      HH.slot' pathToSessions Sessions.Slot Sessions.ui unit absurd
    viewPage (Sessions (Show n)) =
      HH.slot' pathToSessions Sessions.Slot Sessions.ui unit absurd
    viewPage Profile =
      HH.slot' pathToProfile Profile.Slot Profile.ui unit absurd
    viewPage _ =
      HH.div_ []

    eval :: Input ~> H.ParentDSL State Input ChildQuery ChildSlot Void m
    eval (Goto r next) = do
      put r
      pure next
    -- eval (Goto (Sessions view) next) = do
    --   modify case view of
    --               Index -> (_ { currentPage = "Sessions" })
    --               Show n -> (_ { currentPage = "Session " <> show n })
    --   pure next
    -- eval (Goto Home next) = do
    --   modify (_ { currentPage = "Home" })
    --   pure next
testEffects :: forall eff . String -> Aff (HA.HalogenEffects (console :: CONSOLE | eff)) Unit
testEffects str = do
  log str

routeSignal :: forall eff. H.HalogenIO Input Void (Aff (HA.HalogenEffects  (console :: CONSOLE|eff)))
            -> Aff (HA.HalogenEffects (console :: CONSOLE|eff)) Unit
routeSignal driver = do
  Tuple old new <- matchesAff routing
  redirects driver old new
  log "test log in two effects"

redirects :: forall eff. H.HalogenIO Input Void (Aff (HA.HalogenEffects eff))
          -> Maybe Routes
          -> Routes
          -> Aff (HA.HalogenEffects eff) Unit
redirects driver _ =
  driver.query <<< H.action <<< Goto
-- redirects driver _ Home =
--   driver (left (action (Goto Home))))
-- redirects driver _ Profile =
--   driver (left (action (Goto Profile))))
-- redirects driver _ (Sessions view) =
--   driver (left (action (Goto (Sessions view)))))

module STS where

import Prelude

import Data.Lazy (Lazy)
import Data.Lazy as Lazy
import Data.Lens (over)
import Data.Lens.Record (prop)
import Type.Proxy (Proxy(..))
import Data.Lens.Types (Lens)

type Fight =
  { enemies :: Array Enemy
  , drawPile :: Array Card
  , player :: Player
  }

_player :: forall a b r. Lens { player :: a | r } { player :: b | r } a b
_player = prop (Proxy :: Proxy "player")

_block :: forall a b r. Lens { block :: a | r } { block :: b | r } a b
_block = prop (Proxy :: Proxy "block")

_energy :: forall a b r. Lens { energy :: a | r } { energy :: b | r } a b
_energy = prop (Proxy :: Proxy "energy")

type Block = Int
type Energy = Int

type Player = { block :: Block, energy :: Energy }
type Enemy = {}

newtype Card = Card
  { name :: String
  , cost :: Energy
  , ruleText :: RuleText
  }

type RuleText = { text :: String, rule :: Rule }
type Rule = Fight -> Array (PlayerAction)

type PlayerAction = Lazy (Array Outcome)
type Outcome = Lazy Fight

defend :: Card
defend = Card
  { name: "Defend"
  , cost: 1
  , ruleText: gain 6 Block
  }

shrugItOff :: Card
shrugItOff = Card
  { name: "Shrug It Off"
  , cost: 1
  -- TODO: this will not work as expected but looks good
  , ruleText: gain 8 Block <> draw 1 Card
  }

data Gainable
  = Block
  | Energy

gain :: Int -> Gainable -> RuleText
gain amount = case _ of
  Block ->
    { text: "Gain " <> show amount <> " Block."
    , rule: singleOutcome $ over (_player <<< _block) (_ + amount)
    }
  Energy ->
    { text: "Gain " <> show amount <> " Energy."
    , rule: singleOutcome $ over (_player <<< _energy) (_ + amount)
    }

draw :: forall a. Int -> (a -> Card)-> RuleText
draw amount _ =
  { text: case amount of
      1 -> "Darw 1 card."
      _ -> "Darw " <> show amount <> " cards."
  , rule: multipleOutcomes $ \fight -> do
      -- TODO actually do something with the card
      _ <- fight.drawPile
      pure $ pure fight
  }

modifyPlayer :: (Player -> Player) -> Fight -> Fight
modifyPlayer f fight = fight { player = f fight.player }

singleOutcome :: (Fight -> Fight) -> Fight -> Array (PlayerAction)
singleOutcome f fight = [ Lazy.defer \_ -> [ Lazy.defer \_ -> f fight ] ]

multipleOutcomes :: (Fight -> Array Outcome) -> Fight -> Array (PlayerAction)
multipleOutcomes f fight = [ Lazy.defer \_ -> f fight ]
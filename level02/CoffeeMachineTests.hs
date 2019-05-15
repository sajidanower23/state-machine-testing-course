{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}


module CoffeeMachineTests (stateMachineTests) where

import qualified CoffeeMachine          as C
import           Control.Lens           (view, (&))
import           Control.Monad.IO.Class (MonadIO)
import           Data.Kind              (Type)
import           Hedgehog
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range
import           Test.Tasty             (TestTree)
import           Test.Tasty.Hedgehog    (testProperty)

data DrinkType
  = Coffee
  | HotChocolate
  | Tea
  deriving (Enum, Bounded, Eq, Show)

data Model (v :: Type -> Type) = Model DrinkType Bool

data AddMug (v :: Type -> Type) = AddMug deriving Show
data TakeMug (v :: Type -> Type) = TakeMug deriving Show

-- Replace these with one SetDrinkType data type whose constructor
-- takes the type of drink to select as an argument. Don't forget to
-- add a HTraversable instance.

data SetDrinkType (v :: Type -> Type) = SetDrinkType DrinkType deriving Show

instance HTraversable SetDrinkType where
  htraverse _ (SetDrinkType d) = pure $ SetDrinkType d

instance HTraversable AddMug where
  htraverse _ _ = pure AddMug

instance HTraversable TakeMug where
  htraverse _ _ = pure TakeMug

-- Replace the three command definitions above with one cSetDrinkType
-- command definition below, which will the SetDrinkType data type you
-- defined above.

cSetDrinkType
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cSetDrinkType mach = Command gen exec
  [ Update $ \(Model _ hasMug) (SetDrinkType d) _ -> Model d hasMug
  , Ensure $ \_ (Model d _) _ drink -> case (d, drink) of
      (Coffee, C.Coffee{})             -> success
      (Tea, C.Tea{})                   -> success
      (HotChocolate, C.HotChocolate{}) -> success
      _                                -> failure
  ]
  where
    gen :: Model Symbolic -> Maybe (g (SetDrinkType Symbolic))
    gen (Model d _)            = Just $ pure $ SetDrinkType d
    exec :: SetDrinkType Concrete -> m C.Drink
    exec (SetDrinkType d) = do
      mach & case d of
        Coffee       -> C.coffee
        Tea          -> C.tea
        HotChocolate -> C.hotChocolate
      view C.drinkSetting <$> C.peek mach

-- Fill in these command definitions to take and replace the mug in
-- the machine. Use the TakeMug and AddMug types you defined
-- previously.
--
-- You should only return a generator when it makes sense: taking a
-- mug when it's in the machine, or adding a mug when the machine is
-- empty. You will also need `Require` callbacks that enforce this.
--
-- You should also write an `Ensure` callback that verifies that your
-- action worked.

cTakeMug
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cTakeMug mach = Command gen exec
  [
  ]
  where
    gen :: Model Symbolic -> Maybe (g (AddMug Symbolic))
    gen _ = Nothing
    exec :: AddMug Concrete -> m (Either C.MachineError ())
    exec _ = C.addMug mach

cAddMug
  :: forall g m. (MonadGen g, MonadTest m, MonadIO m)
  => C.Machine
  -> Command g m Model
cAddMug mach = Command gen exec
  [
  ]
  where
    gen :: Model Symbolic -> Maybe (g (TakeMug Symbolic))
    gen _ = Nothing
    exec :: TakeMug Concrete -> m (Either C.MachineError C.Mug)
    exec _ = C.takeMug mach


stateMachineTests :: TestTree
stateMachineTests = testProperty "State Machine Tests" . property $ do
  mach <- C.newMachine

  let initialModel = Model HotChocolate False
      commands = ($ mach) <$>
        [ cSetDrinkType
        , cTakeMug
        , cAddMug
        ]

  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialModel commands
  C.reset mach
  executeSequential initialModel actions

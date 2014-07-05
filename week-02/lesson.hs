data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show

shoe :: Thing
shoe = Shoe

data FailableDouble = Failure
                    | OK Double
  deriving Show

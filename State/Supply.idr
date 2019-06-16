module Supply

import Control.Monad.State

data Supply s a = S (State (List s) a)


runSupply : Supply s a -> List s -> (a, List s)


next : Supply s (Maybe s)

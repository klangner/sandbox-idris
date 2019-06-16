module SimpleState


record State s a where
    constructor MkState
    runState : s -> (a, s)


get : State s s
get = MkState $ \s => (s, s)


put : s -> State s ()
put s = MkState (\_ => ((), s))


pure : a -> State s a
pure a = MkState (\s => (a, s))


(>>=) : (State s a) -> (a -> State s b) -> State s b
st >>= f = MkState $ \s =>
    let (a', s') = (runState st) s
    in runState (f a') s'


testState : State Integer (Integer, Integer)
testState = do put 10
               s <- get
               pure (s, s)

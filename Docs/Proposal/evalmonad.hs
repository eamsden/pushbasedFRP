-- SF Evaluation monad transformer

data SFEvalMonadT :: * -> * -> * -> * -> *

instance Monad (SFEvalMonadT svIn svOut m)

instance SFMonadTransformer (SFEvalMonadT svIn svOut)

initSF :: SF svIn svOut -> SFState svIn svOut

runSFEvalMonadT :: (EventOnly svIn, EventOnly svOut, Monad m) => 
                   SFState svIn svOut ->
                   [SVIndex (-> m ()) svOut] ->
                   SFEvalMonadT svIn svOut m a ->
                   m (a, SFState svIn svOut)

push :: (EventOnly svIn, EventOnly svOut) =>
		SVIndex Id svIn ->
		SFEvalMonadT svIn svOut m ()


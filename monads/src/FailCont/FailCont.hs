module FailCont.FailCont where 

-- r is the result 
-- e is the type of a failure
-- a is the type of a success
newtype FailCont r e a = FailCont { runFailCont :: (Either e a -> r) -> r }

toFailCont :: Either e a -> FailCont r e a
toFailCont x = FailCont (\f -> f x)

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont x = runFailCont x id

instance Functor (FailCont r e) where
    -- (a -> b)
    -- (Either e a -> r) -> r
    -- (a -> b) -> ( (Either e a -> r) -> r ) ->  ( (Either e b -> r) -> r )
    fmap x (FailCont f) = FailCont (\g -> f (g . fmap x))


instance Applicative (FailCont r e) where
    -- a -> ( (Either e a -> r) -> r )
    pure a = FailCont (\f -> f (Right a))
    -- ( (Either e (a -> b) -> r) -> r ) -> ( (Either e a -> r) -> r ) -> ( (Either e b -> r) -> r )
    (FailCont f) <*> (FailCont x) = FailCont $ \g ->
        f $ \x1 ->
            case x1 of
                Left e  -> g (Left e)
                Right h -> x (g . fmap h)


instance Monad (FailCont r e) where
    -- ( (Either e a -> r) -> r ) -> ( a -> (Either e b -> r) -> r ) -> (Either e b -> r) -> r
    (FailCont f) >>= x = FailCont $ \g -> 
        f $ \x1 ->
            case x1 of
                Left e -> g (Left e)
                Right h -> runFailCont (x h) g

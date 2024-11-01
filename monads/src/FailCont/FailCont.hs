module FailCont.FailCont where 

-- r is the result 
-- e is the type of a failure
-- a is the type of a success
newtype FailCont r e a = undefined 

toFailCont :: Either e a -> FailCont r e a
toFailCont = undefinedd 

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont = undefined 

instance Functor (FailCont r e) where

instance Applicative (FailCont r e) where

instance Monad (FailCont r e) where

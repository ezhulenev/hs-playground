{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RoleAnnotations            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}


module Lib where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Proxy
import           Data.Reflection
import           Data.Type.Coercion   (Coercion (..))

--import qualified Data.Vector.Unboxed         as V
--import qualified Data.Vector.Unboxed.Mutable as MV

newtype Container p = Container { runContainer :: [Int] }
    deriving (Eq, Show)

coerceConfig :: forall p q . (Reifies p Integer, Reifies q Integer) => Maybe (Coercion (Container p) (Container q))
coerceConfig | reflect (Proxy :: Proxy p) == reflect (Proxy :: Proxy q) = Just Coercion
             | otherwise = Nothing

type role Container nominal

instance Reifies p Integer => Monoid (Container p) where
  mempty = Container $ replicate (fromIntegral (reflect (Proxy :: Proxy p))) 0
  mappend (Container l) (Container r) = Container $ l ++ r


containerCtx :: forall p. Reifies p Integer => Container p -> Integer
containerCtx _ = reflect (Proxy :: Proxy p)

class Add a b where
  type SumTy a b
  add :: a -> b -> SumTy a b

instance Add Integer Double where
  type SumTy Integer Double = Double
  add x y = fromIntegral x + y

instance Add Double Integer where
  type SumTy Double Integer = Double
  add x y = x + fromIntegral y

newtype Pair a b = Pair { runPair :: forall c. (a -> b -> c) -> c }

mkPair :: a -> b -> Pair a b
mkPair a b = Pair $ \f -> f a b

newtype Supply s a = S (State [s] a) deriving (Functor, Applicative, Monad)

next = S $ do st <- get
              case st of
                [] -> return Nothing
                (x:xs) -> do put xs
                             return (Just x)

runSupply (S m) xs = runState m xs

run :: State [Int] Int
run = get >>= (\l -> return $ head l)

r = runReader (ask >>= \x -> return (x * 3)) 2

newtype MyConfig = MyConfig { config :: String } deriving (Show)
newtype MyState = MyState { state :: Int } deriving (Show)

app :: ReaderT MyConfig (State MyState) String
app = do cfg <- ask
         st <-  get
         _ <- put $ MyState 123
         return ((config cfg) ++ (show st))

module Name where

import qualified Data.Map as M

newtype Name = Name Int deriving (Eq, Ord)
instance Show Name where
    show (Name n) = 'n' : show n

next :: Name -> Name
next (Name n) = Name (n + 1)

data NameEnv a = NameEnv (M.Map Name String) Name a deriving (Show, Eq)

instance Functor NameEnv where
    fmap f (NameEnv nameMap nextName a) = NameEnv nameMap nextName (f a)

nameEnv :: (M.Map Name String, Name, a) -> NameEnv a
nameEnv (nameMap, nextName, a) = NameEnv nameMap nextName a

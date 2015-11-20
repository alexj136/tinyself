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

instance Applicative NameEnv where
    pure = NameEnv M.empty (Name 0)
    (NameEnv nameMapF nextNameF f) <*> (NameEnv nameMapA nextNameA a) =
        NameEnv
            (M.unionWithKey
                (\n s1 s2 -> if s1 == s2 then s1 else
                    error "Cannot combine conflicting NameEnvs")
                nameMapF nameMapA)
            (if nextNameF > nextNameA then nextNameF else nextNameA)
            (f a)

nameEnv :: (M.Map Name String, Name, a) -> NameEnv a
nameEnv (nameMap, nextName, a) = NameEnv nameMap nextName a

getValue :: NameEnv a -> a
getValue (NameEnv _ _ a) = a

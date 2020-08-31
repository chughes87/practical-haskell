import qualified Data.Map as M

data InfNumber a = MinusInfinity | Number a | PlusInfinity deriving Show

infMax MinusInfinity x = x
infMax x MinusInfinity = x
infMax PlusInfinity _ = PlusInfinity
infMax _ PlusInfinity = PlusInfinity
infMax (Number a) (Number b) = Number (max a b)

data Person = Person { firstName :: String
                     , lastName :: String
                     } deriving Show

data Client = GovOrg { clientName :: String }
            | Company { clientName :: String
                      , companyId :: Integer
                      , person :: Person
                      , duty :: String }
            | Individual { person :: Person }
            deriving Show

-- compareClient :: Client a -> Client a -> Ordering
compareClient (Individual{person = p1}) (Individual{person = p2})
              = compare (firstName p1) (firstName p2)
compareClient (Individual {}) _ = GT
compareClient _ (Individual {}) = LT
compareClient c1 c2             = compare (clientName c1) (clientName c2)

enum a b | a > b = []
enum a b | a <= b = a : enum (a+1) b

bypassNothing :: ((a -> b) -> Maybe a -> Maybe b)
bypassNothing fn (Just v) = Just $ fn v
bypassNothing fn (Nothing) = Nothing

adjust :: (a -> a) -> String -> (M.Map String a) -> (M.Map String a)
adjust fn key map =
  let newV = fmap fn $ M.lookup key map
  in case newV of
    Nothing -> map
    _ -> M.alter (const newV) key map

adjust2 :: (a -> a) -> String -> (M.Map String a) -> (M.Map String a)
adjust2 fn key map = M.alter (fmap fn) key map

data ClientKind = GovOrgKind | CompanKind | IndividualKind


class Nameable n where
  name :: n -> String

instance Nameable Client where
  name Individual { person = Person { firstName = f, lastName = n } } = f ++ "" ++ n
  name c = clientName c

data BinaryTree2 a = Node2 a (BinaryTree2 a) (BinaryTree2 a) | Leaf2 deriving Show


data BinaryTree3 v c = Node3 v c (BinaryTree3 v c) (BinaryTree3 v c)
  | Leaf3
  deriving (Show, Eq, Ord)

treeInsert3 :: (Ord v, Ord c) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert3 v c (Node3 v2 c2 l r)
  = case compare v v2 of
    EQ -> Node3 v2 c2 l r
    LT -> Node3 v2 (min c c2) (treeInsert3 v c l) r
    GT -> Node3 v2 (min c c2) l (treeInsert3 v c r)
treeInsert3 v c Leaf3 = Node3 v c Leaf3 Leaf3

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- type Mayb a = a -> Maybe a

data Mayb a = Jus a | Nah

instance Functor Mayb where
  fmap fn (Jus a) = Jus $ fn a 
  fmap _ Nah = Nah

instance Functor BinaryTree2 where
  fmap fn (Node2 v l r) = Node2 (fn v) (fmap fn l) (fmap fn r)
  fmap _ Leaf2 = Leaf2



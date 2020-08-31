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


adjust :: ((a -> b) -> Maybe a -> Maybe b) -> String -> (M.Map String a) -> (M.Map String b)
adjust fn key map =
  let newV = bypassNothing fn $ M.lookup key map
  in case newV of
    Nothing -> map
    _ -> M.alter map (const newV) key

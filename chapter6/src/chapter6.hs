{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Data.List
import qualified Data.Map as M
import Control.Lens

-- minimumB :: (a -> a) -> [a] -> a
-- minimumB fn list = foldr (\acc v -> min acc (fn v)) 0 list

class Ord v => Vector v where
  distance :: v -> v -> Double
  centroid :: [v] -> v

instance Vector (Double, Double) where
  distance (a,b) (c,d) = sqrt $ (c-a)*(c-a) + (d-b)*(d-b)
  centroid lst =
    let (u,v) = foldr (\(a,b) (c,d) -> (a+c,b+d)) (0,0) lst
        n = fromIntegral $ length lst
    in (u / n, v / n)

class Vector v => Vectorizable e v where
  toVector :: e -> v

instance Vectorizable (Double, Double) (Double, Double) where
  toVector = id

clusterAssignmentPhase :: (Ord v,  Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points =
  let initialMap = M.fromList $ zip centroids (repeat [])
    in foldr (\p m -> let chosenC = minimumBy (compareDistance p) centroids
        in M.adjust (p:) chosenC m)
        initialMap points
  where compareDistance p x y = compare (distance x $ toVector p)
                                        (distance y $ toVector p)

newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v,v)]
newCentroidPhase = M.toList . fmap (centroid . map toVector)

shouldStop :: (Vector v) => [(v,v)] -> Double -> Bool
shouldStop centroids threshold = foldr (\(x,y) s -> s + distance x y) 0.0 centroids < threshold

kMeans :: (Vector v, Vectorizable e v)
  => (Int -> [e] -> [v])
  -> Int
  -> [e]
  -> Double
  -> [v]
kMeans i k points = kMeans' (i k points) points

kMeans' :: (Vector v, Vectorizable e v) => [v] -> [e] -> Double -> [v]
kMeans' centroids points threshold =
  let assignments = clusterAssignmentPhase centroids points
      oldNewCentroids = newCentroidPhase assignments
      newCentroids = map snd oldNewCentroids
  in if shouldStop oldNewCentroids threshold
    then newCentroids
    else kMeans' newCentroids points threshold

data Client i = GovOrg i String
  | Company i String Person String
  | Individual i Person
data Person = Person String String

firstName :: Lens' Person String
firstName = lens (\(Person f _) -> f)
                (\(Person _ l) newF -> Person newF l)


lastName :: Lens' Person String
lastName = lens (\(Person _ l) -> l)
                (\(Person f _) newL -> Person f newL)

identifier :: Lens (Client i) (Client j) i j
identifier = lens (\case  (GovOrg i _) -> i
                          (Company i _ _ _) -> i
                          (Individual i _) -> i)
                  (\client newId -> case client of
                    GovOrg _ n -> GovOrg newId n
                    Company _ n p r -> Company newId n p r
                    Individual _ p -> Individual newId p)
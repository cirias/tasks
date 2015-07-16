import Data.List

findWaterLine :: [[Integer]] -> (Integer, Integer)
findWaterLine matrix =
    maximumBy compareWaterLine waterLines
  where
    waterLines = zip (nub $ concat matrix) (tail $ nub $ concat matrix)
    compareWaterLine lineA lineB =
        compare (countIsland lineA) (countIsland lineB)
      where
        countIsland (_, upper) =
            length $ findClusters abovePositions
          where
            abovePositions =
                map (\(x, y, _) -> (x, y)) $
                    filter (\(_, _, high) -> high >= upper) points
              where
                xSize = length matrix
                ySize = length $ head matrix
                positions = [(x, y) | x <- [0..(xSize - 1)], y <- [0..(ySize - 1)]]
                points = map (\(x, y) -> (x, y, matrix !! x !! y)) positions

-- Find all clusters with given positions
findClusters :: (Num a, Enum a, Eq a) => [(a, a)] -> [[(a, a)]]
findClusters [] = []
findClusters ps =
    let (cluster, rest) = findOneCluster [] ps
    in cluster : findClusters rest

-- Get first clusters from given postions
findOneCluster :: (Num a, Enum a, Eq a) =>
                      [(a, a)] -> [(a, a)] -> ([(a, a)], [(a, a)])
findOneCluster cluster [] = (cluster, [])
findOneCluster [] (p:ps) = findOneCluster [p] ps
findOneCluster cluster ps
    | null newMembers = (cluster, ps)
    | otherwise = findOneCluster (cluster ++ newMembers) (ps \\ newMembers)
  where newMembers = [p | cp <- cluster, p <- ps, isAdjacent cp p]

-- Test the given positions is adjacent
isAdjacent :: (Num a, Enum a, Eq a) => (a, a) -> (a, a) -> Bool
isAdjacent (x1, y1) (x2, y2) =
    x1 == x2 && y1 == (y2 + 1) ||        -- Right of
    x1 == x2 && y1 == (y2 - 1) ||        -- Left of
    x1 == (x2 + 1) && y1 == y2 ||        -- Down of
    x1 == (x2 - 1) && y1 == y2 ||        -- Up of
    x1 == (x2 + 1) && y1 == (y2 + 1) ||  -- Down right
    x1 == (x2 + 1) && y1 == (y2 - 1) ||  -- Down left
    x1 == (x2 - 1) && y1 == (y2 - 1) ||  -- Up left
    x1 == (x2 - 1) && y1 == (y2 + 1)     -- Up right

m = [[1, 2, 6, 3],
     [5, 2, 4, 3],
     [1, 2, 4, 5],
     [1, 2, 6, 1]]

ps = [(0, 0), (5, 9), (1, 0), (3, 9), (1, 1), (2, 2)]

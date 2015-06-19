import System.Environment
import System.IO
import Data.List

main = do
    (fileName:args) <- getArgs
    content <- readFile fileName
    let rows = tail $ lines content
        matrix = map (map read . words) rows
        maxIslands = findWaterLine matrix
    putStrLn $ show maxIslands
    {-putStrLn $ show matrix-}

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

findWaterLine :: [[Integer]] -> Int
findWaterLine matrix =
    maximum $ map countIsland waterLines
  where
    waterLines = zip (rmdups $ concat matrix) (tail $ rmdups $ concat matrix)
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
findClusters (p:ps) =
    {-let (ads, rest) = findAllAdjacents [p] ps-}
    {-in (p : ads) : findClusters rest-}
    let (cluster, rest) = findAdjacents p ps
    in cluster : findClusters rest

findAllAdjacents :: (Num a, Enum a, Eq a) =>
                      [(a, a)] -> [(a, a)] -> ([(a, a)], [(a, a)])
findAllAdjacents _ [] = ([], [])
findAllAdjacents [] rest = ([], rest)
findAllAdjacents (p:ps) rest =
    let (as, rst) = findAdjacents' p rest
        (ads, rs) = findAllAdjacents ps rst
        (adjs, r) = findAllAdjacents (as ++ ads) rs
    in (as ++ ads ++ adjs, r)

findAdjacents' :: (Num a, Enum a, Eq a) =>
                    (a, a) -> [(a, a)] -> ([(a, a)], [(a, a)])
findAdjacents' _ [] = ([], [])
findAdjacents' p (r:rs)
    | isAdjacent p r = (r:as, rest)
    | otherwise = (as, r:rest)
  where (as, rest) = findAdjacents' p rs

findAdjacents :: (Num a, Enum a, Eq a) =>
                      (a, a) -> [(a, a)] -> ([(a, a)], [(a, a)])
findAdjacents p [] = ([p], [])
findAdjacents p ps
    | null a = ([p], ps)
    | otherwise =
        let
            (as, rst) = case a of (h:_) -> findAdjacents h rest
            (ads, rs) = findAdjacents p rst
        in (a `union` as `union` ads, rs)
  where (a, rest) = findOneAdjacent p ps

findOneAdjacent :: (Num a, Enum a, Eq a) =>
                      (a, a) -> [(a, a)] -> ([(a, a)], [(a, a)])
findOneAdjacent p ps
    | null rest = ([], head)
    | otherwise = case rest of (a:rst) -> ([a], head ++ rst)
  where (head, rest) = span (not . isAdjacent p) ps

-- Test the given positions is adjacent
isAdjacent :: (Num a, Enum a, Eq a) => (a, a) -> (a, a) -> Bool
isAdjacent (x1, y1) (x2, y2) =
    x1 == x2 && y1 == (y2 + 1) ||        -- Right of
    x1 == x2 && y1 == (y2 - 1) ||        -- Left of
    x1 == (x2 + 1) && y1 == y2 ||        -- Down of
    x1 == (x2 - 1) && y1 == y2           -- Up of
    {-x1 == (x2 - 1) && y1 == y2 ||        -- Up of-}
    {-x1 == (x2 + 1) && y1 == (y2 + 1) ||  -- Down right-}
    {-x1 == (x2 + 1) && y1 == (y2 - 1) ||  -- Down left-}
    {-x1 == (x2 - 1) && y1 == (y2 - 1) ||  -- Up left-}
    {-x1 == (x2 - 1) && y1 == (y2 + 1)     -- Up right-}

m = [[1, 2, 6, 3],
     [5, 2, 4, 3],
     [1, 2, 4, 5],
     [1, 2, 6, 1]]

ps = [(0, 0), (5, 9), (1, 0), (3, 9), (1, 1), (2, 2), (1, 2), (0, 1)]

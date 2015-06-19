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

findWaterLine :: [[Int]] -> Int
findWaterLine matrix =
    maximum $ map countIsland waterLines
  where
    waterLines = tail $ rmdups $ concat matrix
    countIsland wl =
        let
            xSize = length matrix
            ySize = length $ head matrix
            initMap = replicate (xSize + 2) $ replicate (ySize + 2) 0
            positions = [(x, y) | x <- [1..xSize], y <- [1..ySize]]
            coloredMap = foldl (color matrix wl) initMap positions
        in (length $ rmdups $ concat coloredMap) - 1

color :: [[Int]] -> Int -> [[Int]] -> (Int, Int) -> [[Int]]
color matrix wl colorMap (x, y) | (matrix !! (x - 1) !! (y - 1)) < wl = colorMap
color matrix wl colorMap p@(x, y)
    | null coloredAdjacents =
        let newColor = 1 + (maximum $ concat colorMap)
        in setColor p newColor colorMap
    | otherwise =
        let minColor =
                getColor colorMap $
                    minimumBy (\p1 p2 -> compare (getColor colorMap p1)
                                                 (getColor colorMap p2))
                              coloredAdjacents
            newMap = setColor p minColor colorMap
            differentColoredAdjacents =
                filter (\p -> (getColor colorMap p) /= minColor) coloredAdjacents
        in if null differentColoredAdjacents
               then newMap
               else foldl (color matrix wl) newMap differentColoredAdjacents
  where
    getColor theMap (x, y) = theMap !! x !! y
    setColor (x, y) c theMap =
        let (headRows, row:tailRows) = splitAt x theMap
            (headNums, num:tailNums) = splitAt y row
        in headRows ++ (headNums ++ c : tailNums) : tailRows
    coloredAdjacents =
        filter (\(x, y) -> (colorMap !! x !! y) > 0) $ adjacents p

adjacents :: (Int, Int) -> [(Int, Int)]
adjacents p@(x, y) =
    map (\f -> f p) directions
  where directions = [ (\(x, y) -> (x - 1, y))
                     , (\(x, y) -> (x, y - 1))
                     , (\(x, y) -> (x + 1, y))
                     , (\(x, y) -> (x, y + 1))
                     ]

m = [[5, 1, 5, 1, 5],
     [5, 1, 5, 4, 5],
     [5, 4, 5, 5, 5],
     [1, 1, 1, 1, 1],
     [1, 1, 1, 5, 1]] :: [[Int]]

ps = [(0, 0), (5, 9), (1, 0), (3, 9), (1, 1), (2, 2), (1, 2), (0, 1)]::[(Int, Int)]

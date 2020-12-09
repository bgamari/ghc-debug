
{-# Language BangPatterns #-}
{-# Language DeriveAnyClass #-}
{-# Language DerivingStrategies #-}
{-# Language DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language ParallelListComp #-}

import System.Environment (getArgs)
import Data.List (foldl', maximumBy, permutations, sortBy)
import Data.Function
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.DeepSeq
import GHC.Generics (Generic)

import GHC.Debug.Stub

import Debug.Trace

data MovieDB = MovieDB
    { users   :: !(Map UserID User)
    , movies  :: !(Map MovieID Movie)
    , ratings :: ![Rating]
    }
    deriving stock (Generic)
    deriving anyclass (NFData)

newtype UserID = UserID Int
    deriving newtype (Eq,Ord,Num,Enum)
    deriving stock (Generic)
    deriving anyclass (NFData)

data User = User
    { name  :: !String
    , trust :: !Double -- ^ How trusted this user is. In range [0,1]. Higher is more trustworthy.
    }
    deriving stock (Generic)
    deriving anyclass (NFData)

newtype MovieID = MovieID Int
    deriving newtype (Eq,Ord,Num,Enum)
    deriving stock (Generic, Show)
    deriving anyclass (NFData)

type Movie = String

data Rating = Rating
    { user   :: !UserID   -- ^ Who rated this movie.
    , movie  :: !MovieID  -- ^ Movie being rated.
    , rating :: !Double   -- ^ Rating In range [1,10]. Higher is better.
    }
    deriving stock (Generic)
    deriving anyclass (NFData)

main :: IO ()
main = withGhcDebug $ do
    -- Parse DB size from arguments
    [nUsersStr, nMoviesStr, nRatingsStr] <- getArgs
    let
        nUsers   = read nUsersStr
        nMovies  = read nMoviesStr
        nRatings = read nRatingsStr

    -- Create the DB
    putStrLn $ "Creating the DB with size ("
                ++ show nUsers ++ " users,"
                ++ show nMovies ++ " movies,"
                ++ show nRatings ++ " ratings)"
    let !movieDB = let
            users = M.fromList
                $ zip
                    [0..]
                    [ User name trust
                    | name <- take nUsers
                        $ fmap unwords
                        $ cycle
                        $ permutations ["abcdefghijklmnopqrstuvwxyz"]
                    | trust <- cycle [0.0, 0.1 .. 1.0]
                    ]

            movies = M.fromList
                $ zip
                    [0..]
                    (take nMovies
                        $ fmap unwords
                        $ cycle
                        $ permutations ["The", "Cat", "In", "The", "Hat", "House", "Mouse", "Story", "Legend"]
                    )

            ratings = take nRatings
                $ cycle
                $ zipWith3
                    Rating
                    (cycle (M.keys users))
                    (cycle (M.keys movies))
                    (cycle [1.0, 1.01 .. 9])

            in force $ MovieDB
                { users = users
                , movies = movies
                , ratings = ratings
                }

    do
        let msg = "DB created."
        traceEventIO msg
        putStrLn msg
    putStrLn "Hit ENTER to continue."
    _ <- getLine


    -- A single user's favorite movie.
    do
        let
            (userIDA, userA) = M.assocs (users movieDB) !! 0
            favoriteMovieID
                = snd
                $ maximumBy (compare `on` fst)
                    [ (rating, movieID)
                    | Rating userIDB movieID rating <- ratings movieDB
                    , userIDA == userIDB
                    ]

        putStrLn $ "User '" ++ name userA ++ "''s favorite movie: " ++ (movies movieDB M.! favoriteMovieID)

    -- Top rated movies
    -- each rating is weighted by the user's trust score.
    do
        let
            weight_movies = foldl' go M.empty (ratings movieDB)

            go :: Map MovieID (Double, Double) -> Rating -> Map MovieID (Double, Double)
            go weights (Rating userID movieID score) = M.alter
                (\case
                    Nothing -> Just (userTrust * score, userTrust)
                    Just (weightedScore, weight) -> let
                        weightedScore' = weightedScore + (userTrust * score)
                        weight'        = weight        + userTrust
                        in Just (weightedScore', weight')
                )
                movieID
                weights
                where
                userTrust = trust ((users movieDB) M.! userID)

        saveClosures [Box weight_movies]
        putStrLn "Pausing"
        getLine

            -- (Movie ID, rating) sorted by rating.
        let movieRatings :: [(MovieID, Double)]
            movieRatings
                = sortBy (compare `on` snd)
                $ fmap (\(movieID, (weightedRating, weight)) -> (movieID, weightedRating / weight))
                $ filter (\(_, (_, weight)) -> weight /= 0)
                $ M.assocs weight_movies

        putStrLn "Top 10 movies:"
        putStrLn $ unlines $ fmap show $ take 10 $ reverse $ movieRatings

    traceEventIO "Top 10 movies done."
    putStrLn "Hit ENTER to continue."
    _ <- getLine

    touch movieDB

{-# NOINLINE touch #-}
touch :: a -> IO ()
touch a = return ()

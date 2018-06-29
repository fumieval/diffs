import Gauge
import qualified Data.ByteString.Char8 as B
import System.IO
import System.IO.Error
import qualified Data.Vector as V
import Data.Diff
import qualified Data.Algorithm.Diff as Diff
import Control.Exception

getRevision h = do
  len <- read <$> hGetLine h
  l <- B.hGet h (len + 1)
  (B.lines l:) <$> getRevision h
  `catch` \e -> if isEOFError e then return [] else throwIO e

trans :: Diff.Diff a -> (Move, a)
trans (Diff.First a) = (L, a)
trans (Diff.Second a) = (R, a)
trans (Diff.Both a _) = (B, a)

main = do
  revisions <- withFile "benchmarks/revisions.txt" ReadMode getRevision
  let benchWith f = nf (uncurry (zipWith f)) (revisions, tail revisions)
  defaultMain
    [ bench "diffs" $ benchWith diff
    , bench "Diff" $ benchWith ((map trans .) . Diff.getDiff)
    ]

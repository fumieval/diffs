import Data.Diff
import Test.QuickCheck
import qualified Data.Vector as V

main :: IO ()
main = quickCheck $ \xs ys -> patch (xs :: [Char]) (diff xs ys) === Just ys

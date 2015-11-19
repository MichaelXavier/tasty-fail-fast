{-# LANGUAGE CPP           #-}
{-# LANGUAGE TupleSections #-}
module Test.Tasty.Ingredients.FailFast
    ( failFast
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Data.IntMap.Strict     as IM
import           Data.Proxy
import           Test.Tasty.Ingredients
import           Test.Tasty.Options
import           Test.Tasty.Runners
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-- | Decorate a TestReporter. Will throw an error if you provide a TestManager.
failFast :: Ingredient -> Ingredient
failFast (TestManager _ _) = error "FailFast must be applied to a TestReporter"
failFast (TestReporter opts f) = TestReporter (ffOpt:opts) f'
  where ffOpt = Option (Proxy :: Proxy FailFast)
        f' oset tree = let FailFast ff = lookupOption oset
                       in if ff
                            then ffHijack <$> f oset tree
                            else f oset tree


-------------------------------------------------------------------------------
ffHijack :: (StatusMap -> IO (Time -> IO Bool)) -> StatusMap -> IO (Time -> IO Bool)
ffHijack f sm = do
  hijackedSM <- IM.fromList <$> mapM mkStatus (IM.keys sm)
  _ <- forkIO (work sm hijackedSM)
  f hijackedSM
  where mkStatus i = (i,) <$> newTVarIO NotStarted

-------------------------------------------------------------------------------
newtype FailFast = FailFast Bool

instance IsOption FailFast where
  defaultValue = FailFast False
  parseValue = fmap FailFast . safeRead
  optionName = return "fail-fast"
  optionHelp = return "Fail the whole suite when the first test fails."
  optionCLParser = flagCLParser Nothing (FailFast True)


-------------------------------------------------------------------------------
work :: StatusMap -> StatusMap -> IO ()
work src dest = atomically $ do
  check =<< anyFailed src
  failAll dest


-------------------------------------------------------------------------------
anyFailed :: StatusMap -> STM Bool
anyFailed = anyM (fmap isFailed . readTVar) . IM.elems
  where isFailed (Done (Result { resultOutcome = Failure _})) = True
        isFailed _                                            = False


-------------------------------------------------------------------------------
anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ []       = return False
anyM p (x:xs)   = do q <- p x
                     if q
                       then return True
                       else anyM p xs
-------------------------------------------------------------------------------
failAll :: StatusMap -> STM ()
failAll = mapM_ failOne . IM.elems

-------------------------------------------------------------------------------
failOne :: TVar Status -> STM ()
failOne = flip modifyTVar' go
  where go NotStarted = Done res
        go x = x
        res = Result { resultOutcome = Failure TestFailed
                     , resultDescription = mempty
#if MIN_VERSION_tasty(0,11,0)
                     , resultShortDescription = mempty
#endif
                     , resultTime = 0}

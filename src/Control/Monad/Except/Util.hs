module Control.Monad.Except.Util (
  tryIO
) where


import Control.Exception (IOException, try)
import Control.Monad.Except (MonadError, MonadIO, liftIO, throwError)
import Data.String (IsString(..))


tryIO :: (IsString e, MonadError e m, MonadIO m) => IO a -> m a
tryIO = (>>= either (\e -> throwError . fromString $ show (e :: IOException)) return) . liftIO . try

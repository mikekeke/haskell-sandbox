module LogUtils where
import Data.Set (member)

data LogType
  = Send
  | Receive
  | Switch
  | DiscardLoop
  deriving stock (Eq, Ord)


log :: Set LogType -> LogType -> Text -> IO ()
log allowed current msg = 
  when (current `member` allowed) (print msg)


dSet = 
  fromList
    [
      Switch
    ]

dLog = log dSet
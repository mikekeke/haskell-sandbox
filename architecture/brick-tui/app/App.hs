{-# LANGUAGE TemplateHaskell #-}

module App (runIO) where


import qualified Brick.Main as M
import Screens.Start (StartDialog, startDialog, drawStartDialog, dialogEvent, startDialogMap, Name)
import  Brick.Types







data AppState
  = StartScreen {_sDialog :: StartDialog}

suffixLenses ''AppState


drawUI :: AppState -> [Widget Name]
drawUI = \case 
  StartScreen s -> drawStartDialog s

appEvent :: BrickEvent Name e -> EventM Name AppState ()
appEvent ev = do
  st <- get
  case st of
    (StartScreen _) -> zoom _sDialogL $ dialogEvent ev



theApp :: M.App AppState e Name
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return ()
          , M.appAttrMap = const startDialogMap
          }

runIO :: IO ()
runIO = do
    d <- M.defaultMain theApp (StartScreen startDialog)
    pure ()
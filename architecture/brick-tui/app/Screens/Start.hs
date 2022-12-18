module Screens.Start where

import Prelude hiding (on)
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import qualified Brick.Types as T
import Brick (BrickEvent (VtyEvent), Widget, padAll, str)
import qualified Graphics.Vty as V
import qualified Brick.Main as M
import Brick.Util (on, bg)

type StartDialog =  D.Dialog Choice Name

data Choice = Register | ListUsers | ListCargos
            deriving Show

data Name =
    RegisterButton
    | ListUsersButton
    | ListCargosButton
    deriving (Show, Eq, Ord)

drawStartDialog :: D.Dialog Choice Name -> [Widget Name]
drawStartDialog d = [ui]
    where
        ui = D.renderDialog d $ C.hCenter $ padAll 1 $ str "This is the dialog body."

dialogEvent :: BrickEvent Name e -> T.EventM Name (D.Dialog Choice Name) ()
dialogEvent (VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> M.halt
        V.EvKey V.KEnter [] -> M.halt
        _ -> D.handleDialogEvent ev
dialogEvent _ = return ()

startDialog :: D.Dialog Choice Name
startDialog = D.dialog (Just $ str "Title") (Just (RegisterButton, choices)) 50
    where
        choices = [ ("Register",   RegisterButton,   Register)
                  , ("List users",  ListUsersButton,  ListUsers)
                  , ("List cargos", ListCargosButton, ListCargos)
                  ]

startDialogMap :: A.AttrMap
startDialogMap = A.attrMap V.defAttr
    [ (D.dialogAttr, V.white `on` V.blue)
    , (D.buttonAttr, V.black `on` V.white)
    , (D.buttonSelectedAttr, bg V.yellow)
    ]

module UI.PickLevel
  ( pickLevel,
  )
where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Brick.Widgets.Center (hCenter)
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import System.Exit (exitSuccess)

app :: App (Maybe Int) e ()
app =
  App
    { appDraw = const [ui],
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const $ attrMap V.defAttr [],
      appChooseCursor = neverShowCursor
    }

ui :: Widget ()
ui =
  padLeft (Pad 19) $
    padRight (Pad 21) $
      C.center $
        vLimit 22 $
          hLimit 60 $
            withBorderStyle BS.unicodeBold $
              B.borderWithLabel (str " WELCOME TO ") $
                C.center $
                  vBox
                    [ C.hCenter $
                        vBox
                          [ str "  _____     __      __   ",
                            str " / ___/__ _/ /_____/ /   ",
                            str "/ /__/ _ `/ __/ __/ _ \\  ",
                            str "\\___/\\_,_/\\__/\\__/_//_/ "
                          ],
                      C.hCenter $
                        vBox
                          [ str "   _  ___ ",
                            str "  / |/ ( )",
                            str " /    /|/ ",
                            str "/_/|_/    "
                          ],
                      C.hCenter $
                        vBox
                          [ str "   ___          __        ",
                            str "  / _ \\___  ___/ /__ ____ ",
                            str " / // / _ \\/ _  / _ `/ -_)",
                            str "/____/\\___/\\_,_/\\_, /\\__/ ",
                            str "               /___/      "
                          ],
                      padTop (Pad 6) $ C.hCenter $ str "Pick Level 0-9"
                    ]

handleEvent :: Maybe Int -> BrickEvent () e -> EventM () (Next (Maybe Int))
handleEvent n (VtyEvent (V.EvKey V.KEsc _)) = halt n
handleEvent n (VtyEvent (V.EvKey (V.KChar 'q') _)) = halt n
handleEvent n (VtyEvent (V.EvKey (V.KChar 'Q') _)) = halt n
handleEvent n (VtyEvent (V.EvKey (V.KChar d) [])) =
  if d `elem` ['0' .. '9']
    then halt $ Just (read [d])
    else continue n
handleEvent n _ = continue n

pickLevel :: IO Int
pickLevel = defaultMain app Nothing >>= maybe exitSuccess return

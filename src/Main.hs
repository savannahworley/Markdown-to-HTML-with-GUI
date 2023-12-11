{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}



module Main where

import Control.Lens
import Data.Text
import Monomer
import TextShow
import MarkdownToHTMLConverter

import qualified Monomer.Lens as L

data AppModel = AppModel {
  _textInputMKD :: Text,
  _textOutput :: Text
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | TextProcess
  deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  enterText = keystroke [("Enter", TextProcess)] $ 
    vstack [
    hstack [label "Enter Your Markdown Here:", mainButton "Convert" TextProcess],
      spacer,
      textArea inputMKD,
      hstack [label "Here is Your Parsed Markdown: "],
      textArea output
    ]
  widgetTree = zstack [
     vstack [
      enterText
     ]
    ] `styleBasic` [padding 10]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  TextProcess -> [
    Model (model & output .~ pack (runConverter(textToString (model ^. inputMKD))))
    ]

textToString :: Text -> String
textToString t = unpack t


main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Markdown to HTML Generator",
      appWindowIcon "./assets/images/icon.png",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent AppInit
      ]
    model = AppModel "" ""

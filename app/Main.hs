module Main where

import Chess.Update
import Chess.View

import Data.Char (toLower)
import System.Environment ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> humanVsHumanFromFEN LightTheme startingFEN
    themeStr : modeStr : args' -> do
      let fen = case args' of
                  []       -> startingFEN
                  fen' : _ -> fen'

      let tryCmd = do
            theme <- case map toLower themeStr of
                        "lighttheme" -> Right LightTheme
                        "darktheme"  -> Right DarkTheme
                        _       -> Left (unlines [ "Invalid theme: " ++ themeStr
                                                 , "Theme must be 'lighttheme' or 'darktheme'"
                                                 ])

            case map toLower modeStr of
              "humanvshuman"              -> Right (humanVsHumanFromFEN theme fen)
              "humanvsstockfish"          -> Right (humanVsStockfishFromFEN theme fen)
              "stockfishvsstockfish"      -> Right (stockfishVsStockfishFromFEN theme fen)
              "stockfishvsstockfishquick" -> Right (stockfishVsStockfishQuickFromFEN theme fen)
              "stockfishopeningsdemo"     -> Right (stockfishOpeningsDemo theme)
              _ -> Left (unlines [ "Invalid mode: " ++ modeStr
                                 , "Mode must be 'humanvshuman', 'humanvsstockfish', 'stockfishvsstockfish', 'stockfishvsstockfishquick', or 'stockfishopeningsdemo'"
                                 ])
      case tryCmd of
        Right cmd -> cmd
        Left errStr -> putStrLn errStr

    _ -> putStrLn "Unexpected input format. Try: haskell-chess darktheme humanvsstockfish"

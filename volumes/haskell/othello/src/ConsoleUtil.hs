module ConsoleUtil where

import qualified TypeDefs

-- 画面クリア
cls :: String
cls = "\ESC[2J"

-- カーソル移動
goto :: TypeDefs.Pos -> String
goto (x, y) = "\ESC[" ++ (show y) ++ ";" ++ (show x) ++ "H"

-- 一文字吐くやつ
writec :: TypeDefs.Pos -> String -> String
writec p xs = goto p ++ xs

-- カラーで文字吐くやつ
writeClrC :: TypeDefs.Pos -> Color -> String -> String
writeClrC p fg xs = goto p ++ ctrlCFg fg ++ xs ++ ctrlCFg CWhite

-- コンソール用のカラーコード吐くやつ
data Color = CBlack | CRed | CGreen | CYellow | CBlue | CMage | CCyan | CWhite

colorCodeFg :: Color -> Int
colorCodeFg CBlack  = 30
colorCodeFg CRed    = 31
colorCodeFg CGreen  = 32
colorCodeFg CYellow = 33
colorCodeFg CBlue   = 34
colorCodeFg CMage   = 35
colorCodeFg CCyan   = 36
colorCodeFg CWhite  = 37

ctrlCFg :: Color -> String
ctrlCFg c = "\ESC[" ++ show (colorCodeFg c) ++ "m"

colorCodeBg :: Color -> Int
colorCodeBg CBlack  = 40
colorCodeBg CRed    = 41
colorCodeBg CGreen  = 42
colorCodeBg CYellow = 43
colorCodeBg CBlue   = 44
colorCodeBg CMage   = 45
colorCodeBg CCyan   = 46
colorCodeBg CWhite  = 47

ctrlCBg :: Color -> String
ctrlCBg c = "\ESC[" ++ show (colorCodeBg c) ++ "m"

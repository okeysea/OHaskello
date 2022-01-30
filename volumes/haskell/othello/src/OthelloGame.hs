module OthelloGame (doMain) where

import qualified ConsoleUtil as CU
import qualified TypeDefs
import System.IO (stdin, stdout, hReady, hSetEcho, hSetBuffering, BufferMode (NoBuffering))
import Control.Monad.IO.Class
import Data.Bits -- ビット操作

data InputKey = KUp | KDown | KRight | KLeft | KSpace | KUnknown
data GameSequences = GsOpening | GsInGame
data GameStates = GameStates{
                              gsSequence :: GameSequences
                            , gsCurPos   :: TypeDefs.Pos
                            , gsBoard    :: TypeDefs.Board
                            }

--
-- 副作用さんたち
--

doMain :: IO ()
doMain = do
  ioSetting
  othelloGame =<< initGameState

-- バッファリングされないように設定
ioSetting :: IO ()
ioSetting = do
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho      stdin  False

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where
    getKey' chars = do
      char <- getChar
      more <- hReady stdin
      (if more then getKey' else return) (char:chars)

convKey :: String -> InputKey
convKey k =
  case k of
    "\ESC[A"  -> KUp
    "\ESC[B"  -> KDown
    "\ESC[C"  -> KRight
    "\ESC[D"  -> KLeft
    otherwise -> KUnknown

initGameState :: IO GameStates
initGameState = return $ GameStates{gsSequence=GsOpening,gsCurPos=(0,0),gsBoard=initBoard}

-- オセロ初期配置を返す
initBoard :: TypeDefs.Board
initBoard = TypeDefs.Board 0x0000001008000000 0x0000000810000000

othelloGame :: GameStates -> IO ()
othelloGame st = do
  putStr $ CU.cls
  putStr $ dispBoard $ gsBoard st
  putStr $ dispCursor st
  putStr $ CU.goto (0, 0)
  c <- getKey
  othelloGame $ updateGameState (convKey c) st

--
-- 純粋関数の世界
--
pieceBlack = "x"
pieceWhite = "o"
sepHeadRow = "+---+---+---+---+---+---+---+---+"
sepRow     = "|   |   |   |   |   |   |   |   |"
boardDispX = 1
boardDispY = 1
pieceMulStX = 4
pieceMulStY = 2

-- ボードを表示する
dispBoard :: TypeDefs.Board -> String
dispBoard b = drawBoard ++ drawPiece b

-- ボードの背景を描画する
drawBoard :: String
drawBoard = drawBoard' 8
  where
    drawBoard'   0 = drawBoardHeadSep (boardDispX, boardDispY + ( 2 * 8 ))
    drawBoard' row = drawBoard' (row - 1) ++ drawBoardHeadSep (x1, y1) ++ drawBoardSep (x2, y2)
      where
        x1 = boardDispX
        x2 = boardDispX
        y1 = (row * 2) + boardDispY - 2
        y2 = (row * 2) + boardDispY - 1

drawBoardHeadSep :: TypeDefs.Pos -> String
drawBoardHeadSep (x, y) = CU.writeClrC (x, y) CU.CWhite sepHeadRow

drawBoardSep :: TypeDefs.Pos -> String
drawBoardSep (x, y) = CU.writeClrC (x, y) CU.CWhite sepRow


-- piece を描画する
drawPiece :: TypeDefs.Board -> String
drawPiece b = drawPiece' b 0
  where
    drawPiece' b 64  = ""
    drawPiece' b idx = (black) ++ (white) ++ (drawPiece' b (idx + 1))
      where
        black = if blackCheck idx b
                   then CU.writeClrC (idxToDispPos idx) CU.CRed pieceBlack
                   else ""
        white = if whiteCheck idx b
                   then CU.writeClrC (idxToDispPos idx) CU.CWhite pieceWhite
                   else ""

blackCheck :: Int -> TypeDefs.Board -> Bool
blackCheck i b = bitCheck i $ TypeDefs.b_o1 b 

whiteCheck :: Int -> TypeDefs.Board -> Bool
whiteCheck i b = bitCheck i $ TypeDefs.b_o2 b 

-- ビットがiの位置に立っているかチェックする
bitCheck :: Int -> TypeDefs.BoardBitTable -> Bool
bitCheck i b = b .&. (shift 1 i) > 0

-- idx を 表示座標に変換する
idxToDispPos :: Int -> TypeDefs.Pos
idxToDispPos idx = posToDispPos $ idxToPos idx

-- Idxを pos に 変換する
idxToPos :: Int -> TypeDefs.Pos
idxToPos idx = (x, y)
  where
    x = idx `mod` 8
    y = (idx - ( idx `mod` 8 )) `div` 8

-- Pos を表示位置を算出する
posToDispPos :: TypeDefs.Pos -> TypeDefs.Pos
posToDispPos (x, y) = (x', y')
  where
    x' = (x * pieceMulStX) + boardDispX + 2
    y' = (y * pieceMulStY) + boardDispY + 1

posToIdx :: TypeDefs.Pos -> Int
posToIdx (x, y) = 8 * y + x

dispCursor :: GameStates -> String
dispCursor st = drawCursor (gsCurPos st) (gsBoard st)

drawCursor :: TypeDefs.Pos -> TypeDefs.Board -> String
drawCursor p b = (black) ++ (white) ++ (blank)
  where
    black = if blackCheck (posToIdx p) b
               then CU.writeClrC (idxToDispPos (posToIdx p)) CU.CGreen pieceBlack
               else ""
    white = if whiteCheck (posToIdx p) b
               then CU.writeClrC (idxToDispPos (posToIdx p)) CU.CGreen pieceWhite
               else ""
    blank = if not ((blackCheck (posToIdx p) b) || (whiteCheck (posToIdx p) b))
               then CU.writeClrC (idxToDispPos (posToIdx p)) CU.CGreen "C"
               else ""

-- ゲームステートを更新
updateGameState :: InputKey -> GameStates -> GameStates
updateGameState ik gs = 
  case ik of
    KUp      -> gs{gsCurPos=(acc ( 0, -1) (gsCurPos gs))}
    KDown    -> gs{gsCurPos=(acc ( 0,  1) (gsCurPos gs))}
    KRight   -> gs{gsCurPos=(acc ( 1,  0) (gsCurPos gs))}
    KLeft    -> gs{gsCurPos=(acc (-1,  0) (gsCurPos gs))}
    KUnknown -> gs
  where
    acc (x, y) (x', y') = (maxOrMin(x+x'),maxOrMin(y+y'))
      where
        maxOrMin n
          | n > 7     = 7
          | n < 0     = 0
          | otherwise = n

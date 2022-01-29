module TypeDefs where

import Data.Word

type Pos    = (Int, Int)

type BoardBitTable = Word64

data Board  = Board { b_o1 :: BoardBitTable
                    , b_o2 :: BoardBitTable
                    }

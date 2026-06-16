module Main (main) where

import qualified Data.ByteString as ByteString
import Data.Foldable
import Data.Word
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Pqi
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Prelude

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "unescapeBytea" do
    for_
      [ "",
        "\\x",
        "\\x00",
        "\\x00ff",
        "\\x48656c6c6f",
        "\\X48656C6C6F",
        "\\xAbCd",
        "Hello, world",
        "h\233llo bytes",
        "\\\\",
        "\\001\\002\\003",
        "a\\010b",
        "\\x4",
        "\\x4g",
        "\\xzz",
        "\\x61 62",
        "\\377",
        "\\400",
        "\\000",
        "\\1",
        "\\18",
        "\\8",
        "a\\b",
        "trailing\\",
        "mixed\\134text"
      ]
      \input ->
        it (show input) do
          theirs <- LibPQ.unescapeBytea input
          theirs `shouldBe` Just (Pqi.unescapeBytea input)

    -- PQunescapeBytea treats its input as a null-terminated C string, so
    -- embedded NUL bytes would truncate it early. Exclude them.
    prop "matches PQunescapeBytea on arbitrary input" $
      \(bytes :: [Word8]) ->
        ioProperty do
          let input = ByteString.pack bytes
          theirs <- LibPQ.unescapeBytea input
          return $ theirs === Just (Pqi.unescapeBytea input)

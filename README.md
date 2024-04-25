# Haskell Function Graph

A form of "Hoogle type search" for functions. Enables finding a sequence of composed functions that converts from one type to another. For example, searching for "lazy Text to strict ByteString" returns e.g. `bytestring-0.11.4.0:Data.ByteString.Char8.pack . text-2.0.2:Data.Text.Lazy.unpack`, `text-2.0.2:Data.Text.Encoding.encodeUtf8 . text-2.0.2:Data.Text.Lazy.toStrict` and `bytestring-0.11.4.0:Data.ByteString.toStrict . text-2.0.2:Data.Text.Lazy.Encoding.encodeUtf8`.

## Running

Run

```
nix-shell --run 'cabal run server -- 8080 data/all3.json'
```

and visit http://localhost:8080.

## Demo

![](docs/img/demo.gif)

## TODO

Run the shell script `TODO.sh` to list unfinished TODOs.

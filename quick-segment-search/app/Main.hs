module Main where

import           AliMagF
import           Control.Lens
import           Data.Aeson                 (encode)
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.List
import           Data.Monoid
import           FastDipole
import           System.Environment
import           System.FilePath.Posix      (takeBaseName)
import           System.Posix               (fileExist)

main :: IO ()
main = do
  (prefix, name, path) <- parseArgs <$> getArgs
  let tmpfile = prefix <> "_tmp.dat" -- store intermediate result for dev
  hasIntermediateResult <- fileExist tmpfile
  (result, len) <-
    if hasIntermediateResult then
      read <$> readFile tmpfile
    else do
      putStrLn "Parsing input file..."
      params <- parseAliMagWrapCheb path
      let jsonName = takeBaseName path <> ".json"
      putStrLn $ "Writing " <> jsonName <> " (used to generate the correspoinding header)"
      C.writeFile jsonName $ encode params -- write json here
      case params of
        Nothing       -> error "No parse."
        Just (_, dat) -> do
          putStrLn "Generating header file. It may take tens of minutes..."
          let result = (fastDipoleSegs $ dipoleParams dat, length (dipoleParams dat))
          writeFile tmpfile $ show result
          return result
  writeFile (name <> ".cxx") $ renderAll prefix name result len

parseArgs :: [String] -> (String, String, FilePath)
parseArgs [] = ("dip5k", "AliMagFastDip5k", "../../AliMagF/Sol30_Dip6_Hole.txt")
parseArgs [tag, name, txtname] = (tag, name, txtname)
parseArgs _ = error "Usage: genparam dip5k AliMagFastDip5k Sol30_Dip6_Hole.txt"

renderArrayContent :: [String] -> String
renderArrayContent xs = "{" <> intercalate ", " xs <> "}"

renderArrayDef :: String -> String -> [String] -> String
renderArrayDef typename ident xs = mconcat
  [ typename, " ", ident, "[] = {\n  "
  , intercalate ",\n  " xs
  , "\n};\n" ]

renderLineDivision, renderLineDivision' :: String -> LineDivision -> String
renderLineDivision prefix (LineDivision n w o) = renderArrayContent
  [ show n, show (realToFrac n/w), show o, prefix <> "_slices", prefix <> "_segments" ]
renderLineDivision' prefix (LineDivision n w o) = renderArrayContent
  [ show n, show (realToFrac n/w), show o, prefix <> "_slices"
  , "(AliMagFast::SegmentSearch_t*)" <> prefix <> "_ids" ]

renderSegmentEnd :: SegmentEnd -> String
renderSegmentEnd (SegmentEnd index endPos) = renderArrayContent [show index, show endPos]

renderRootDef :: String -> LineDivision -> String
renderRootDef ident dv = "AliMagFast::SegmentSearch_t " <> ident <> " = " <> renderLineDivision ident dv <> ";\n"

renderSlicesDef :: String -> [SegmentEnd] -> String
renderSlicesDef prefix = renderArrayDef "AliMagFast::SegmentEnd_t" (prefix <> "_slices") . map renderSegmentEnd

seqIdent :: String -> String -> [String]
seqIdent prefix suffix = [prefix <> show i <> suffix | i <- [0..]]

renderAll :: String -> String -> FastDipole -> Int -> String
renderAll prefix name ss len =
  "#include \"AliMagFast.h\"\n"
  <> ("#include \"" <> name <> ".h\"\n")
  <> "typedef unsigned short ushort;\n"
  <> "const float Infinity = INFINITY;\n"
  <> renderZDef (prefix <> "_z") ss
  <> renderArrayDef "AliMagFast::ChebFormula_t" (prefix <> "_params")
      ((\i -> renderArrayContent [i <> "bz", i]) . (prefix <>) . show <$> [0..len - 1])

renderZDef :: String -> FastDipole -> String
renderZDef prefix ss = mconcat
  [ mconcat $ zipWith renderXDef idents $ _fSegments ss
  , renderSlicesDef prefix $ _fSlices ss
  , renderArrayDef "AliMagFast::SegmentSearch_t" (prefix <> "_segments") .
      zipWith (\i ss -> renderLineDivision i $ _fDivision ss) idents $ _fSegments ss
  , renderRootDef prefix $ _fDivision ss
  ] where idents = seqIdent prefix "_x"

renderXDef :: String -> FastDipoleX -> String
renderXDef prefix ss = mconcat
  [ mconcat $ zipWith renderYDef idents $ _fSegments ss
  , renderSlicesDef prefix $ _fSlices ss
  , renderArrayDef "AliMagFast::SegmentSearch_t" (prefix <> "_segments") .
      zipWith (\i ss -> renderLineDivision' i $ _fDivision ss) idents $ _fSegments ss
  ] where idents = seqIdent prefix "_y"

renderYDef :: String -> FastDipoleY -> String
renderYDef prefix ss = mconcat
  [ renderSlicesDef prefix $ _fSlices ss
  , renderArrayDef "ushort" (prefix <> "_ids") . map (show . head) $ _fSegments ss
  ]

-- | debug
load :: IO ([(String, AliCheb3D)], FastDipole)
load = do
  Just (_, a) <- parseAliMagWrapCheb "Sol30_Dip6_Hole.txt"
  let dip = dipoleParams a
  let s = fastDipoleSegs dip
  -- s ^.. fSegments . traverse . fSegments . traverse . fSegments . traverse . to length
  --let r = quickSearch s (-1000)
  return (dip, s)

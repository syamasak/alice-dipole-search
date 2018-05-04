{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module AliMagF (
-- exports
  parseAliMagWrapCheb, AliMagWrapCheb(..), AliCheb3D(..), AliCheb3DCalc(..),
  solenoidParams_, tpcIntParams_, tpcRatIntParams_, dipoleParams_,
  absprec, region, outputs,
  cpc, polprec
  ) where
import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Aeson
import GHC.Generics
import Text.Trifecta
import Text.Parser.Combinators

-- | Reads AliMagWrapCheb::SaveData() file.
parseAliMagWrapCheb :: FilePath -> IO (Maybe (String, AliMagWrapCheb))
parseAliMagWrapCheb = parseFromFile aliMagWrapCheb

-- | Parser combinator that parse (block name, AliMagWrapCheb)
aliMagWrapCheb :: Parser (String, AliMagWrapCheb)
aliMagWrapCheb = do
  skipSpaces
  block $ do
    ("SOLENOID", a) <- block (pieces alicheb3d)
    ("TPCINT", b) <- block (pieces alicheb3d)
    ("TPCRatINT", c) <- block (pieces alicheb3d)
    ("DIPOLE", d) <- block (pieces alicheb3d)
    return $ AliMagWrapCheb a b c d

-- | A Datatype that represents AliMagWrapCheb class in AliRoot.
data AliMagWrapCheb = AliMagWrapCheb {
    solenoidParams :: [(String, AliCheb3D)]
  , tpcIntParams :: [(String, AliCheb3D)]
  , tpcRatIntParams :: [(String, AliCheb3D)]
  , dipoleParams :: [(String, AliCheb3D)]
} deriving (Show, Read, Generic)

-- derive JSON read/write instances by using 'Generic' typeclass
instance ToJSON AliMagWrapCheb
instance FromJSON AliMagWrapCheb

comment :: Parser String
comment = char '#' >> many (notChar '\n')

skipSpaces :: Parser ()
skipSpaces = skipOptional $ many (comment <|> some (oneOf " \t\r\n"))

block :: Parser a -> Parser (String, a)
block p = do
  string "START "
  name <- many (notChar '\n') <* skipSpaces
  body <- p
  string "END " >> string name >> skipSpaces
  return (name, body)

anonymousBlock :: Parser a -> Parser a
anonymousBlock p = fmap snd (block p)

pieces :: Parser a -> Parser [a]
pieces p = do
  numberOfPieces <- int <* skipSpaces
  count numberOfPieces p

int :: Parser Int
int = fmap fromIntegral natural

float :: Parser Double
float = (char '+' >> double) <|> (char '-' >> fmap negate double)

alicheb3d :: Parser (String, AliCheb3D)
alicheb3d =
  block $ do
    dimensions <- int <* skipSpaces
    -- interpolation info
    absPrec <- float <* skipSpaces
    lowerBounds <- count dimensions (float <* skipSpaces)
    upperBounds <- count dimensions (float <* skipSpaces)
    table <- count dimensions $ anonymousBlock $ do
      numberOfRows <- int <* skipSpaces
      columnsPerRow <- count numberOfRows (int <* skipSpaces)
      coefsPerBlock <- forM columnsPerRow (\cols -> count cols (int <* skipSpaces))
      coefficients <- forM coefsPerBlock $ \cols ->
                forM cols $ \coefs ->
                  count coefs (float <* skipSpaces)
      precision <- float <* skipSpaces
      return $ AliCheb3DCalc coefficients precision
    return $ AliCheb3D absPrec (lowerBounds, upperBounds) table

-- | For AliCheb3D class
data AliCheb3D = AliCheb3D
  { interpolationAbsolutePrecision :: Double
  , interpolationRegion :: ([Double], [Double])
  , interpolationOutputs :: [AliCheb3DCalc]
  } deriving (Show, Read, Generic)

instance ToJSON AliCheb3D
instance FromJSON AliCheb3D

-- | For AliCheb3DCalc class
data AliCheb3DCalc = AliCheb3DCalc
  { chebyshevPolynomialCoeffs :: [[[Double]]]
  , chebyshevPolynomialPrecision :: Double
  } deriving (Show, Read, Generic)

instance ToJSON AliCheb3DCalc
instance FromJSON AliCheb3DCalc

-- Lens (generalized composable accessors) are optional
-- [("field name","lens name")]
makeLensesFor [("interpolationAbsolutePrecision","absprec"),("interpolationRegion","region"),("interpolationOutputs","outputs")] ''AliCheb3D
makeLensesFor [("chebyshevPolynomialCoeffs","cpc"),("chebyshevPolynomialPrecision","polprec")] ''AliCheb3DCalc
makeLensesFor [("solenoidParams","solenoidParams_"),("tpcIntParams","tpcIntParams_"),("tpcRatIntParams","tpcRatIntParams_"),("dipoleParams","dipoleParams_")] ''AliMagWrapCheb

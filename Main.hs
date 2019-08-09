{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Codec.Picture
import Codec.Picture.Types
import Control.Lens
import Control.Monad.ST
import Data.Random hiding (sample)
import Data.Random.Extras
import Options.Applicative
import System.FilePath.Lens


main :: IO ()
main = do
  cli@CLI {..} <- either (error . show) id . validateCLI <$> execParser cliParser
  src <- either (error "Couldn't read image") convertRGBA8 <$> readImage cImgPath
  swapOpts <- makeSwapOpts cli src
  let baseDir  = cImgPath ^. directory
      fileName = cImgPath ^. filename
      new = makeImage swapOpts src
  writePng (baseDir <> "/" <> show cColRand <> "_" <> show cRowRand <> "-" <> fileName) new
  where
    validateCLI cli@(CLI _ c r) = if (c < 0 || c > 100) || (r < 0 || r > 100)
      then Left "Options -c and -r should be integers in the range [0, 100]." else Right cli
    cliParser = info (helper <*> parseCLI) (header "glitch-art")
    parseCLI = CLI
      <$> strOption (long "file" <> help "Image to sort")
      <*> option auto (short 'c' <> help "Integer in the range 0-100 representing the percentage of columns to be swapped")
      <*> option auto (short 'r' <> help "Integer in the range 0-100 representing the percentage of rows to be swapped")


-- | CLI.
data CLI = CLI
  { cImgPath :: FilePath -- ^ Path to the file to process.
  , cColRand :: Int -- ^ Degree of randomness to apply to column swapping.
  , cRowRand :: Int -- ^ Degree of randomness to apply to row swapping.
  } deriving (Eq, Show)


-- | Configuration for image generation.
data SwapOpts = SwapOpts
  { soCol :: Maybe [(Int, Int)] -- ^ List of column indices to swap.
  , soRow :: Maybe [(Int, Int)] -- ^ List of row indices to swap.
  } deriving (Eq, Show)


-- | Create the configuration for image generation from CLI options.
makeSwapOpts
  :: CLI -- ^ User-provided options.
  -> Image PixelRGBA8 -- ^ Source image.
  -> IO SwapOpts
makeSwapOpts (CLI _ c r) Image {..} = do
  soCol <- if c == 0 then return Nothing else Just <$> makeSwapOpt c imageWidth
  soRow <- if r == 0 then return Nothing else Just <$> makeSwapOpt r imageHeight
  return SwapOpts {..}
  where makeSwapOpt rand dim = do
          let n' = floor $ fromIntegral (dim * rand) / 100
              n = case n' `mod` 2 of
                0 -> n'
                1 -> n' + 1
                _ -> error "Mathematics is broken."
          randSample <- runRVar (sample n [0..dim - 1]) StdRandom :: IO [Int]
          return $ uncurry zip $ splitAt (floor $ fromIntegral n / 2) randSample


-- | Make the final output image.
makeImage
  :: SwapOpts -- ^ Randomness options.
  -> Image PixelRGBA8 -- ^ Source image.
  -> Image PixelRGBA8
makeImage SwapOpts {..} src = case (soCol, soRow) of
  (Nothing, Nothing) -> src
  (Just co, Nothing) -> swapWith co swapColumns src
  (Nothing, Just ro) -> swapWith ro swapRows src
  (Just co, Just ro) -> swapWith ro swapRows (swapWith co swapColumns src)


-- | Swap row or column indices given a source image and a canvas.
type Swap = Int -> Int -> Image PixelRGBA8 -> Image PixelRGBA8 -> Image PixelRGBA8


-- | Swap a list of indices with a given swapping function.
swapWith
  :: [(Int, Int)] -- ^ Column or row indices to swap.
  -> Swap -- ^ Swapping function.
  -> Image PixelRGBA8 -- ^ Input image.
  -> Image PixelRGBA8
swapWith ixs f img = go ixs f img img
  where go [] _ _ acc                   = acc
        go ((ix1, ix2):ixs') f' src acc = go ixs' f' src (f' ix1 ix2 src acc)


-- | Swap the column with index @c1@ with the column with index @c2@.
swapColumns :: Swap
swapColumns c1 c2 src@Image {..} canvas = runST $ do
  mimg <- unsafeThawImage canvas
  go 0 mimg
  where go !r !mimg
          | r >= imageHeight = unsafeFreezeImage mimg
          | otherwise = do
              writePixel mimg c2 r (pixelAt src c1 r)
              go (r + 1) mimg


-- | Swap the row with index @r1@ with the row with index @r2@.
swapRows :: Swap
swapRows r1 r2 src@Image {..} canvas = runST $ do
  mimg <- unsafeThawImage canvas
  go 0 mimg
  where go !c !mimg
          | c >= imageWidth = unsafeFreezeImage mimg
          | otherwise = do
              writePixel mimg c r2 (pixelAt src c r1)
              go (c + 1) mimg

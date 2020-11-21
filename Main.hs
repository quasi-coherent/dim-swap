{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Codec.Picture
import Codec.Picture.Types
import Control.Lens
import Control.Monad.ST
import Data.List.Split
import Data.Maybe
import Data.Random hiding (sample)
import Data.Random.Extras
import Options.Applicative
import System.FilePath.Lens


main :: IO ()
main = do
  cli@CLI {..} <- either (error . show) id . validateCLI <$> execParser cliParser
  src <- either (error "Couldn't read image") convertRGBA8 <$> readImage cImgPath
  swapOpts <- makeSwapOpts cli src
  let new = case cNumIters of
        Nothing -> makeImage swapOpts src
        Just n  -> foldr ($) src (replicate n (makeImage swapOpts))
      outPath = makeFileName cli cImgPath
  writePng outPath new
  putStrLn outPath
  where
    makeFileName CLI {..} imgPath =
      let baseDir     = imgPath ^. directory
          [name, ext] = case splitOn "." $ imgPath ^. filename of
            (n:x:_) -> [n, x]
            _       -> error "Invalid filename/extension."
      in baseDir <> "/" <> name <> "-swapped-"
          <> show cColRand <> "-" <> show cColLowerBound <> "-" <> show cColUpperBound <> "_"
          <> show cRowRand <> "-" <> show cRowLowerBound <> "-" <> show cRowUpperBound <> "x"
          <> show (fromMaybe 1 cNumIters) <> "." <> ext

    validateCLI cli@(CLI _ c cMin cMax r rMin rMax _)
      | any (\arg -> arg < 0 || arg > 100) [c, cMin, cMax, r, rMin, rMax]
        = Left "Options should be integers in the range [0, 100]."
      | (cMin > cMax) || (rMin > rMax)
        = Left "Column/row minimum must be less than or equal to the column/row maximum."
      | otherwise = Right cli

    cliParser = info (helper <*> parseCLI) (header "dim-swap")

    parseCLI = CLI
      <$> strOption (long "file" <> help "Image to swap")
      <*> option auto (short 'c' <> help "Integer in the range 0-100 representing the percentage of columns to be swapped")
      <*> option auto (long "col-min" <> help "Integer in the range 0-100 representing where to start random column swapping")
      <*> option auto (long "col-max" <> help "Integer in the range 0-100 representing where to end random column swapping")
      <*> option auto (short 'r' <> help "Integer in the range 0-100 representing the percentage of rows to be swapped")
      <*> option auto (long "row-min" <> help "Integer in the range 0-100 representing where to start random row swapping")
      <*> option auto (long "row-max" <> help "Integer in the range 0-100 representing where to end random row swapping")
      <*> optional (option auto $ long "num-iters" <> help "Number of times to run swapping with the given options")


-- | CLI.
data CLI = CLI
  { cImgPath       :: FilePath -- ^ Path to the file to process.
  , cColRand       :: Int -- ^ Degree of randomness to apply to column swapping.
  , cColLowerBound :: Int -- ^ Where to start the column randomness (normalized to the interval [0, 100]).
  , cColUpperBound :: Int -- ^ Where to end the column randomness (normalized to the interval [0, 100]).
  , cRowRand       :: Int -- ^ Degree of randomness to apply to row swapping.
  , cRowLowerBound :: Int -- ^ Where to start the row randomness (normalized to the interval [0, 100]).
  , cRowUpperBound :: Int -- ^ Where to end the row randomness (normalized to the interval [0, 100]).
  , cNumIters      :: Maybe Int -- ^ Number of times to apply the same swapping settings to the same image interatively.
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
makeSwapOpts (CLI _ cRand cMin cMax rRand rMin rMax _) Image {..} = do
  soCol <- if cRand == 0 then return Nothing else Just <$> makeSwapOpt cRand cMin cMax imageWidth
  soRow <- if rRand == 0 then return Nothing else Just <$> makeSwapOpt rRand rMin rMax imageHeight
  return SwapOpts {..}
  where makeSwapOpt rand dMin dMax dim = do
          let rStart = floor $ fromIntegral (dMin * dim) / 100
              rEnd = floor $ fromIntegral (dMax * dim) / 100
              n' = floor $ fromIntegral ((rEnd - rStart) * rand) / 100
              n = case n' `mod` 2 of
                0 -> n'
                1 -> n' + 1
                _ -> error "Mathematics is broken."
          randSample <- runRVar (sample n [rStart..rEnd - 1]) StdRandom :: IO [Int]
          return . uncurry zip $ splitAt (floor $ fromIntegral n / 2) randSample


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

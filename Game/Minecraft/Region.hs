module Game.Minecraft.Region where

import Codec.Compression.Zlib
import Control.Applicative
import Control.Monad
import Data.Bits
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Serialize
import qualified Data.Serialize.Builder as Builder
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe
import Data.Word
import System.FilePath

import Data.NBT
import Game.Minecraft.Block

-- | The (X,Z) coordinates specifying a 'Chunk'
type ChunkCoords = (Int, Int)

-- | The (X,Z) coordinates specifying a 'Region'
type RegionCoords = (Int, Int)

-- | A region contains a collection of 'Chunk's
-- TODO: Replace bytestring with actual chunk data
data Region = Region (Vector (Maybe Chunk))
            deriving Show

data Chunk = Chunk L.ByteString

instance Show Chunk where 
  show _ = "<chunk>"

data Location = Loc Int Word8

getWord24be :: Get Word32
getWord24be = do
  s <- getBytes 3
  return $! (fromIntegral (s `S.index` 0) `shift` 16) .|.
            (fromIntegral (s `S.index` 1) `shift`  8) .|.
            (fromIntegral (s `S.index` 2) )

putWord24be :: Putter Word32
putWord24be w | w `shift` (-24) /= 0 =
                  error "tried to put word larger than 24 bits"
              | otherwise = putBuilder $ Builder.fromByteString bytes
  where bytes = S.pack [ (fromIntegral (w `shift` (-16)) :: Word8)
                       , (fromIntegral (w `shift`  (-8)) :: Word8)
                       , (fromIntegral (w)               :: Word8)
                       ]

instance Serialize Location where
  get = Loc . fromIntegral <$> getWord24be <*> getWord8
  put (Loc offset sectorCount) = 
    putWord24be (fromIntegral offset) >> put sectorCount

getLocations :: Get (Vector Location)
getLocations = V.replicateM 1024 (get :: Get Location)

-- | Don't care about timestamps yet
getTimestamps :: Get ()
getTimestamps = replicateM_ 1024 (get :: Get Word32)

getChunks :: (Vector Location, L.ByteString) -> Vector (Maybe Chunk)
getChunks (locV, chunkData) = V.map getChunk locV
  where
    getChunk (Loc 0 0)                = mzero
    getChunk (Loc offset sectorCount) = 
      return . Chunk . either error id . runGetLazy extractChunk $
             L.take (4096 * (fromIntegral sectorCount))
                    (L.drop (4096 * (fromIntegral (offset - 2))) chunkData)
    extractChunk = do 
      len <- fromIntegral <$> getWord32be
      compScheme <- getWord8
      case compScheme of
        1 -> fail "GZip-compressed chunks not supported"
        2 -> decompress . L.fromChunks . (:[]) <$> ensure (len-1)



getRawRegion :: Get (Vector Location, L.ByteString)
getRawRegion = do
  locV <- getLocations
  getTimestamps
  chunkData <- getLazyByteString . fromIntegral =<< remaining
  return (locV, chunkData)

instance Serialize Region where
  get = do raw <- getRawRegion
           return $ Region (getChunks raw)
  put (Region vector)
      = do let getBs (Chunk bs) = bs
               pad4k s = let (q, r) = quotRem s 4096 in if r == 0 then q else q + 1
               comps   = map (fmap $ compress . getBs) (V.toList vector)
               lengths = map (maybe 0 L.length) comps
               lenSec  = map (pad4k . (+5)) lengths
               starts  = scanl (+) 2 lenSec
               locations = zipWith (\a b -> if b == 0 then Loc 0 0 else Loc (fromIntegral a) (fromIntegral b)) starts lenSec

           mapM_ put locations
           replicateM_ 1024 (putWord32be 0)
           forM_ (zip3 comps lengths lenSec) $ \(c,len,sec) -> do
               when (isJust c) $ do
                   let Just d = c
                   putWord32be $ fromIntegral (len+1)
                   putWord8 2
                   put d
                   replicateM_ (fromIntegral (4096*sec - len - 5)) (putWord8 0)


-- | Given 'ChunkCoords', gives back the 'RegionCoords' containing
-- that chunk
chunkToRegionCoords :: ChunkCoords -> RegionCoords
chunkToRegionCoords (x, z) = (x `shift` (-5), z `shift` (-5))

-- | Given 'RegionCoords', gives back the filename of the region file
-- containing that region
regionFileName :: RegionCoords -> FilePath
regionFileName (x, z) = "r" <.> show x <.> show z <.> "mcr"

testRegion = decode <$> S.readFile ("testWorld/region" </> regionFileName (-1,-1)) :: IO (Either String Region)

testChunk = do (Right (Region v)) <- testRegion
               let (Just (Chunk c)) = (V.!) v 1023
                   (Right nbt) = decodeLazy c
               return (nbt :: NBT)

testBlocks = do (CompoundTag _ [(CompoundTag (Just "Level") ts)]) <- testChunk
                return $ filter (\t -> case t of (ByteArrayTag (Just "Blocks") _ _) -> True; _ -> False) ts

testBlockIds :: IO [BlockId]
testBlockIds = do [(ByteArrayTag _ _ bs)] <- testBlocks
                  return (map (toEnum . fromIntegral) (S.unpack bs))
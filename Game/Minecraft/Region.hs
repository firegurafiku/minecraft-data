module Game.Minecraft.Region (
        ChunkCoords,
        RegionCoords,
        Region(..),
        Chunk(..),
        Location(..),
        chunkToRegionCoords,
        regionFileName,
        loadRegion,
        saveRegion)
where

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

import qualified Data.NBT as NBT
import Game.Minecraft.Block

-- | The (X,Z) coordinates specifying a 'Chunk'
type ChunkCoords = (Int, Int)

-- | The (X,Z) coordinates specifying a 'Region'
type RegionCoords = (Int, Int)

-- | A region contains a collection of 'Chunk's
data Region = Region (Vector (Maybe Chunk))
            deriving Show

data Chunk = Chunk NBT.NBT

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

pad4k s = let (q, r) = quotRem s 4096 in if r == 0 then q else q + 1
getLocations :: Get (Vector Location)
getLocations = V.replicateM 1024 (get :: Get Location)

-- | Don't care about timestamps yet
getTimestamps :: Get ()
getTimestamps = replicateM_ 1024 (get :: Get Word32)

getChunks :: (Vector Location, L.ByteString) -> Vector (Maybe Chunk)
getChunks (locV, chunkData) = V.map getChunk locV
  where
    getChunk :: Location -> Maybe Chunk
    getChunk (Loc 0 0)                = Nothing
    getChunk (Loc offset sectorCount) = Just $ Chunk (either error id $ either error decodeLazy bs)
        where bs :: Either String L.ByteString
              bs = runGetLazy extractChunk $
                   L.take (4096 * (fromIntegral sectorCount))
                      (L.drop (4096 * (fromIntegral (offset - 2))) chunkData)

    extractChunk = do 
      len <- fromIntegral <$> getWord32be
      compScheme <- getWord8
      case compScheme of
        1 -> fail "GZip-compressed chunks not supported"
        2 -> decompress . L.fromChunks . (:[]) <$> ensure (len-1)

encodeChunk :: Chunk -> L.ByteString
encodeChunk chunk = encodeLazy chunkNbt
    where Chunk chunkNbt = chunk

getRawRegion :: Get (Vector Location, L.ByteString)
getRawRegion = do
  locV <- getLocations
  getTimestamps
  chunkData <- getLazyByteString . fromIntegral =<< remaining
  return (locV, chunkData)

instance Serialize Region where
  get = do raw <- getRawRegion
           return $ Region (getChunks raw)

  put region = do
           mapM_ put locations
           -- Don't care about timestamps yet.
           replicateM_ 1024 (putWord32be 0)

           forM_ (zip3 compressedChunks chunkLengths chunkLengthsInSec)
             $ \(mbChunk, len, sec) -> do
               when (isJust mbChunk) $ do
                   let Just chunk = mbChunk
                       paddingSize = fromIntegral (4096*sec - len - 5)

                   putWord32be $ fromIntegral (len+1)
                   putWord8 2 -- compression method
                   put $ encode chunk
                   replicateM_ paddingSize (putWord8 0)

         where
           getBs (Chunk bs) = bs
           Region vector = region
           compressedChunks  = map (fmap $ compress . encodeChunk) (V.toList vector)
           chunkLengths      = map (maybe 0 $ fromIntegral . L.length) compressedChunks
           chunkLengthsInSec = map (pad4k . (+5)) chunkLengths
           chunkOffsetsInSec = scanl (+) 2 chunkLengthsInSec
           locations         = zipWith getLocation chunkOffsetsInSec chunkLengths

           getLocation :: Int -> Int -> Location
           getLocation offset size = if size == 0
                                     then Loc 0 0
                                     else Loc (fromIntegral offset) (fromIntegral size)

-- | Given 'ChunkCoords', gives back the 'RegionCoords' containing
-- that chunk
chunkToRegionCoords :: ChunkCoords -> RegionCoords
chunkToRegionCoords (x, z) = (x `shift` (-5), z `shift` (-5))

-- | Given 'RegionCoords', gives back the filename of the region file
-- containing that region
regionFileName :: RegionCoords -> FilePath
regionFileName (x, z) = "r" <.> show x <.> show z <.> "mcr"

loadRegion :: FilePath -> IO (Either String Region)
loadRegion filename = decode <$> S.readFile filename

saveRegion :: FilePath -> Region -> IO ()
saveRegion filename region = S.writeFile filename (encode region)

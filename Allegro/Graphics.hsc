{-
 - Hurry is a Haskell binding to the Allegro game library.
 - Copyright (c) 2012 Damien Radtke - www.damienradtke.org
 -
 - This library is free software; you can redistribute it and/or
 - modify it under the terms of the GNU Lesser General Public
 - License as published by the Free Software Foundation; either
 - version 3.0 of the License, or (at your option) any later version.
 -
 - This library is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 - Lesser General Public License for more details.
 -
 - For more information, visit http://www.gnu.org/copyleft
 -}

module Allegro.Graphics
( Bitmap
) where

#include <allegro5/allegro.h>

import Allegro.Util

import Control.Monad
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

data BitmapStruct
type Bitmap = Ptr (BitmapStruct)

data ColorStruct
type Color = Ptr (ColorStruct)

newtype DrawFlag = DrawFlag { unDrawFlag :: CInt }
#{enum DrawFlag, DrawFlag
 , flipHorizontal = ALLEGRO_FLIP_HORIZONTAL
 , flipVertical = ALLEGRO_FLIP_VERTICAL
 }

createBitmap :: Int -> Int -> IO (Bitmap)
createBitmap w h = alCreateBitmap (toEnum w) (toEnum h)
foreign import ccall "allegro5/allegro.h al_create_bitmap"
	alCreateBitmap :: CInt -> CInt -> IO (Bitmap)

createSubBitmap :: Bitmap -> (Int,Int) -> Int -> Int -> IO (Bitmap)
createSubBitmap parent (x,y) w h = alCreateSubBitmap parent (toEnum x) (toEnum y) (toEnum w) (toEnum h)
foreign import ccall "allegro5/allegro.h al_create_sub_bitmap"
	alCreateSubBitmap :: Bitmap -> CInt -> CInt -> CInt -> CInt -> IO (Bitmap)

cloneBitmap :: Bitmap -> IO (Bitmap)
cloneBitmap = alCloneBitmap
foreign import ccall "allegro5/allegro.h al_clone_bitmap"
	alCloneBitmap :: Bitmap -> IO (Bitmap)

destroyBitmap :: Bitmap -> IO ()
destroyBitmap = alDestroyBitmap
foreign import ccall "allegro5/allegro.h al_destroy_bitmap"
	alDestroyBitmap :: Bitmap -> IO ()

-- TODO: get/set bitmap flags and options

getBitmapHeight :: Bitmap -> IO (Int)
getBitmapHeight b = liftM fromEnum $ alGetBitmapHeight b
foreign import ccall "allegro5/allegro.h al_get_bitmap_height"
	alGetBitmapHeight :: Bitmap -> IO (CInt)

getBitmapWidth :: Bitmap -> IO (Int)
getBitmapWidth b = liftM fromEnum $ alGetBitmapWidth b
foreign import ccall "allegro5/allegro.h al_get_bitmap_width"
	alGetBitmapWidth :: Bitmap -> IO (CInt)

loadBitmap :: String -> IO (Maybe Bitmap)
loadBitmap filename = withCString filename $ \f -> do
	b <- alLoadBitmap f
	return $ if b == nullPtr then Nothing else Just b
foreign import ccall "allegro5/allegro.h al_load_bitmap"
	alLoadBitmap :: CString -> IO (Bitmap)

drawBitmap :: Bitmap -> (Float,Float) -> [DrawFlag] -> IO ()
drawBitmap b (x,y) flags = alDrawBitmap b (realToFrac x) (realToFrac y) flags'
	where flags' = combineFlags $ fmap unDrawFlag flags
foreign import ccall "allegro5/allegro.h al_draw_bitmap"
	alDrawBitmap :: Bitmap -> CFloat -> CFloat -> CInt -> IO ()

mapRGB :: Word -> Word -> Word -> IO (Color)
mapRGB r g b = alMapRGB (fromIntegral r) (fromIntegral g) (fromIntegral b)
foreign import ccall "allegro5/allegro.h al_map_rgb"
	alMapRGB :: CUChar -> CUChar -> CUChar -> IO (Color)

mapRGBF :: Float -> Float -> Float -> IO (Color)
mapRGBF r g b = alMapRGBF (realToFrac r) (realToFrac g) (realToFrac b)
foreign import ccall "allegro5/allegro.h al_map_rgb_f"
	alMapRGBF :: CFloat -> CFloat -> CFloat -> IO (Color)

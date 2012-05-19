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

module Allegro.Display
( Display
) where

import Foreign.Ptr

data DisplayStruct
type Display = Ptr (DisplayStruct)

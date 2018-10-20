{-# LINE 1 "src/Foreign/Lua/Core/Constants.hsc" #-}
{-|
Module      : Foreign.Lua.Core.Constants
Copyright   : © 2007–2012 Gracjan Polak,
                2012–2016 Ömer Sinan Ağacan,
                2017-2018 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <tarleb+hslua@zeitkraut.de>
Stability   : beta
Portability : ForeignFunctionInterface

Lua constants
-}
module Foreign.Lua.Core.Constants
  ( multret
  , registryindex
  , refnil
  , noref
  ) where

import Foreign.Lua.Core.Types




-- | Alias for C constant @LUA_MULTRET@. See
-- <https://www.lua.org/manual/5.3/#lua_call lua_call>.
multret :: NumResults
multret = NumResults $ -1
{-# LINE 29 "src/Foreign/Lua/Core/Constants.hsc" #-}

-- | Alias for C constant @LUA_REGISTRYINDEX@. See
-- <https://www.lua.org/manual/5.3/#3.5 Lua registry>.
registryindex :: StackIndex
registryindex = StackIndex $ -1001000
{-# LINE 34 "src/Foreign/Lua/Core/Constants.hsc" #-}

-- | Value signaling that no reference was created.
refnil :: Int
refnil = -1
{-# LINE 38 "src/Foreign/Lua/Core/Constants.hsc" #-}

-- | Value signaling that no reference was found.
noref :: Int
noref = -2
{-# LINE 42 "src/Foreign/Lua/Core/Constants.hsc" #-}

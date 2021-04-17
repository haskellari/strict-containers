{-# LANGUAGE CPP, FlexibleContexts, DataKinds, MonoLocalBinds #-}

module Data.Strict.IntMap.Autogen.Internal.DeprecatedDebug where
import Data.Strict.IntMap.Autogen.Internal (IntMap)

import Data.Strict.ContainersUtils.Autogen.TypeError


-- | 'showTree' has moved to 'Data.Strict.IntMap.Autogen.Internal.Debug.showTree'
showTree :: Whoops "Data.Strict.IntMap.Autogen.showTree has moved to Data.Strict.IntMap.Autogen.Internal.Debug.showTree"
         => IntMap a -> String
showTree _ = undefined

-- | 'showTreeWith' has moved to 'Data.Strict.IntMap.Autogen.Internal.Debug.showTreeWith'
showTreeWith :: Whoops "Data.Strict.IntMap.Autogen.showTreeWith has moved to Data.Strict.IntMap.Autogen.Internal.Debug.showTreeWith"
             => Bool -> Bool -> IntMap a -> String
showTreeWith _ _ _ = undefined

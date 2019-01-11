{-# LANGUAGE PolyKinds #-}

module Data.ProxyProxy where

asProxyProxyTypeOf :: proxy1 a -> proxy2 a -> proxy1 a
asProxyProxyTypeOf s _ = s
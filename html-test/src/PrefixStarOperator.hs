{-# LANGUAGE TypeOperators, NoStarIsType #-}
module PrefixStarOperator where
type (*) a = (,) a

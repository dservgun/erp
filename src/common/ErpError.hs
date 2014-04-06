-----------------------------------------------------------------------------
--
-- Module      :  ErpError
-- Copyright   :
-- License     :  GPL Nothing
--
-- Maintainer  :  dinkar.ganti@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module ErpError (
    ErpError(..)
) where


data ErpError a b = Error a | Success b deriving Show





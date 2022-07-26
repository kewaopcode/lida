{-# LANGUAGE TemplateHaskell #-}

module LuaJIT.TH where

import           Control.Lens        ((.~))
import           Control.Lens.TH     (DefName (TopName), lensField, lensRules,
                                      makeLensesWith)
import           Data.Function       ((&))
import           Language.Haskell.TH (DecsQ, Name, mkName, nameBase)


makeLenses_ :: Name -> DecsQ
makeLenses_ =
    makeLensesWith (lensRules & lensField .~ (\_ _ name -> [TopName $ mkName $ "_" ++ nameBase name]))

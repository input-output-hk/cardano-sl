module Pos.Ssc.GodTossing.Utils
       ( -- verifiedVssCertificates
       ) where

-- import           Control.Lens                      ((^.))
-- import           Data.HashMap.Strict               (union)
-- import           Universum

-- import           Pos.Binary.Ssc                    ()
-- import           Pos.Constants                     (k)
-- import           Pos.Ssc.Class.Storage             (SscStorageMode)
-- import           Pos.Ssc.GodTossing.Genesis        (genesisCertificates)
-- import           Pos.Ssc.GodTossing.Types.Base     (VssCertificatesMap)
-- import           Pos.Ssc.GodTossing.Types.Instance ()
-- import           Pos.Ssc.GodTossing.Types.Type     (SscGodTossing)
-- import           Pos.Ssc.GodTossing.Types.Types    (gsVssCertificates)
-- import           Pos.State                         (WorkModeDB, getGlobalMpcDataByDepth)

-- -- | Gets all verified VssCertificates, i.e. those in the genesis block
-- -- and those at least k blocks deep.
-- verifiedVssCertificates ::    (SscStorageMode SscGodTossing , WorkModeDB SscGodTossing m)
--                            => m VssCertificatesMap
-- verifiedVssCertificates = do
--     md <- getGlobalMpcDataByDepth k
--     return $ case md of
--         Nothing -> genesisCertificates
--         Just d  -> (d ^. gsVssCertificates) `union` genesisCertificates

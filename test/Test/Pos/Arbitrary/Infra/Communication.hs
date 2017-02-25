import           Test.QuickCheck (Arbitrary (..))
import           Data.DeriveTH                    (derive, makeArbitrary)
import           Node.Message                     (MessageName (..))
import           Test.QuickCheck                  (Arbitrary (..), choose, oneof)
import           Universum

import           Pos.Communication.Types          (HandlerSpec (..), VerInfo (..))
import           Pos.Communication.Types.SysStart (SysStartRequest (..),
                                                   SysStartResponse (..))

instance (Arbitrary key, Arbitrary tag) => Arbitrary (ReqMsg key tag) where
    arbitrary = ReqMsg <$> arbitrary <*> arbitrary

instance (Arbitrary key, Arbitrary tag) => Arbitrary (InvMsg key tag) where
    arbitrary = InvMsg <$> arbitrary <*> arbitrary

derive makeArbitrary ''SysStartRequest
derive makeArbitrary ''SysStartResponse
derive makeArbitrary ''MessageName

instance Arbitrary HandlerSpec where
    arbitrary = oneof
        [ ConvHandler <$> arbitrary
        , pure OneMsgHandler
        , UnknownHandler <$> choose (2, 255) <*> arbitrary
        ]

derive makeArbitrary ''VerInfo

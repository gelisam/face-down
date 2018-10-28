module Data.Ratio.Extra where

import Codec.FFmpeg (AVRational(AVRational))
import Control.Lens
import Data.Ratio
import Foreign.C.Types


fromAVRational :: AVRational -> Ratio CInt
fromAVRational (AVRational num denom) = num % denom

toAVRational :: Ratio CInt -> AVRational
toAVRational ratio = AVRational (numerator ratio)
                                (denominator ratio)


ratioComponents :: Integral b
                => Traversal (Ratio a) (Ratio b) a b
ratioComponents f ratio = (%) <$> f (numerator   ratio)
                              <*> f (denominator ratio)

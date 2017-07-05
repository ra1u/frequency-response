{-# language TypeSynonymInstances #-}
{-# language LiberalTypeSynonyms #-}

module FrequencyResponse 
  (Analyzer (..),
  AnalyzerAcyclic (..),
  Spectrum (..),
  SpecLin (..),
  SpecWide ,
  getResponse,
  getResponseLin,
  getResponseAcyclic)

where

import CLaSH.Prelude (Default(..))
import Prelude
import CLaSH.Signal.MultiSignal
import Data.Complex as C
import Data.Maybe
import Control.Applicative
import Data.Map.Strict as Map


-- | Type representing input and output signal in circuitry
-- p of kind (* -> *) represents underlaying Signal of type Prependable (ZipList for example) 
-- s is type of Spectrum
data Analyzer p s = Analyzer {unAnalyzer :: ([Maybe (s -> s)] , p s)}


instance (Functor p, Prependable p) => Prependable (Analyzer p) where
     prepend a da = Analyzer (Nothing : f , (prepend a $ fmap fx d))
      where
        (f,d) = unAnalyzer da
        fx = fromMaybe id $ foldr1 (<|>) f

instance (Applicative p,Num n) => Num (Analyzer p n) where
    (+) = liftA2FA (+)
    (-) = liftA2FA (-)
    (*) = liftA2FA (*)
    fromInteger = makeAnalyzer . fromInteger
    negate = liftA1FA negate
    abs = liftA1FA abs
    signum = liftA1FA signum

instance (Applicative p,Fractional n) => Fractional (Analyzer p n) where
    (/) = liftA2FA (/)
    fromRational = makeAnalyzer . fromRational

liftA2FA op da db = Analyzer (zipWith (<|>) fa fb, liftA2 op a b) where
       (fa,a) = unAnalyzer da
       (fb,b) = unAnalyzer db

liftA1FA op f = Analyzer (repeat Nothing , fmap op s)   where
   (_,s) = unAnalyzer f

makeAnalyzer s = Analyzer ( repeat Nothing , pure s )
{- inline liftA2FA, liftA1FA-}
     
-- | spectrum is result of frequency mixing two spectrums
-- n represents type represented as harmonics (Int or better, Rational)
-- and a is type of complex number (frequently Double)
data Spectrum n a = Spectrum {getSpectrum :: Map.Map n (Complex a)}

instance (Ord n,Num n,Default a,RealFloat a) => Default (Spectrum n a) where
    def = dcSpectrum def

instance (Ord n,Num n,RealFloat a) => Num (Spectrum n a) where
    (+) = dotMergeSpec (+)
    (-) = dotMergeSpec (-)
    (*) = mixSpectrum
    negate = fmapSpec negate
    abs = error "no abs function for spectrum available"
    signum = error "no sig num function for spectrum available"
    fromInteger = dcSpectrum . fromInteger

instance (Show n, Show a, Ord n,Num n,Fractional a,RealFloat a) => Fractional (Spectrum n a) where
    (/) = spectrumDiv
    fromRational = dcSpectrum . fromRational

dotMergeSpec op (Spectrum sa) (Spectrum sb) = Spectrum $ (Map.unionWith op) sa sb
fmapSpec f = Spectrum . fmap f . getSpectrum
constSpectrum freq a = Spectrum $ Map.singleton freq (a :+ 0 )
dcSpectrum a = constSpectrum 0 a

mixSpectrum (Spectrum sa) (Spectrum sb) = Spectrum 
        $ Map.fromListWith (+) 
        $ concat [mixFeq a b | a <- Map.toList sa , b <- Map.toList sb]

mixFeq (fa,pa) (fb,pb)
   | fa > fb = mixFeqSorted (fb,pb) (fa,pa)
   | True =  mixFeqSorted (fa,pa) (fb,pb)

mixFeqSorted (0,pa) (fb,pb) = [(fb,pa*pb)]
mixFeqSorted (fa,pa) (fb,pb) = [(fb - fa,r1),(fb+fa,r2)] where
   (ma,pha) = polar pa
   (mb,phb) = polar pb
   k = ma * mb / 2
   r1 
      | fa == fb  = mkPolar (k * sin (phb - pha + pi/2)) 0
      | otherwise = mkPolar k (phb - pha + pi/2)
   r2 = mkPolar ((-1)*k) (phb + pha + pi/2)

delaySpec ph (Spectrum s) = Spectrum $ Map.mapWithKey f s where
    f 0 a = a
    f k a = a * mkPolar 1 (fromRational k * ph)

spectrumDiv (Spectrum a) sb = Spectrum (fmap (/ dc) a) where
    dc = getDcOrError sb
    
getDcOrError (Spectrum a) =
     case toList a of
            ((0,dc):[]) -> dc
            s  -> error ("spectrum is not dc, but" ++ show s )

makeDcSig :: (Applicative p, Num n, Num a) => a -> Analyzer p (Spectrum n a)
makeDcSig = makeAnalyzer . dcSpectrum


-- | spectrum for linear system, faster version of Spectrum
-- underlaying type is tuple insted of Map 
newtype SpecLin = SpecLin {getSpecLin :: (Double,Complex Double)}

instance  Default SpecLin where
    def = SpecLin (def,def :+ def)

instance  Num SpecLin where
    (+) = dotMergeSpecLin (+)
    (-) = dotMergeSpecLin (-)
    (*) = mixSpecLin
    negate = fmapSpecLin negate
    abs = error "no abs function for spectrum available"
    signum = error "no signum function for spectrum available"
    fromInteger = dcSpecLin . fromInteger

instance Fractional SpecLin where
    (/) = undefined -- dcSpecLin
    fromRational = dcSpecLin . fromRational

dotMergeSpecLin op (SpecLin (dcA,acA)) (SpecLin (dcB,acB)) = SpecLin (op dcA dcB , acr) where
    acr = op (realPart acA) (realPart acB) :+ op (imagPart acA) (imagPart acB)

mixSpecLin (SpecLin (dcA,acA)) (SpecLin (dcB,acB)) 
   | (acA /= 0 ) && (acB /= 0) = error "can not mix two linear spectrum"
   | otherwise = SpecLin (dcr , acr) where
       dcr = dcA * dcB
       acr = fmap (* dcA) acB + fmap (* dcB) acA

dcSpecLin dc = SpecLin (dc,0) 

fmapSpecLin f (SpecLin (dc,ac)) =  SpecLin (f dc,fmap f ac)

delayLinSpec ph (SpecLin (dc,ac)) = SpecLin (dc,ac * mkPolar 1 ph)
-- end of SpecLin

-- for acyclic circuits we can implement faster version of Prependable
-- It has nice property that first value of output is correct. 
data AnalyzerAcyclic s = AnalyzerAcyclic {unAnalyzerAcyclic :: (Maybe (s -> s), s)}

-- Prependable instance has funny laws (no laws?)
-- it does not follow list isomorphism, because it is single value
instance Prependable AnalyzerAcyclic where
     prepend _ da = AnalyzerAcyclic (f , fx d)
      where
        (f,d) = unAnalyzerAcyclic da
        fx = fromMaybe id f

instance (Num n) => Num (AnalyzerAcyclic n) where
    (+) = opAcyclic (+)
    (-) = opAcyclic (-)
    (*) = opAcyclic (*)
    fromInteger = pureAcyclic . fromInteger
    negate = liftAcyclic negate
    abs = liftAcyclic abs
    signum = liftAcyclic signum

instance (Fractional n) => Fractional (AnalyzerAcyclic n) where
    (/) = opAcyclic (/)
    fromRational = pureAcyclic . fromRational

pureAcyclic n = AnalyzerAcyclic (Nothing,n)
opAcyclic op (AnalyzerAcyclic (fa,a)) (AnalyzerAcyclic (fb,b)) = (AnalyzerAcyclic (fa <|> fb ,op a b))
liftAcyclic f (AnalyzerAcyclic (fa,a)) = (AnalyzerAcyclic (fa,f a))

-- end of AnalyzerAcyclic

type SpecWide = Spectrum Rational Double

-- | run analysis for both acyclic\/cyclic and both linear\/nonlinear circuits
-- It is required to skip first N resulting values
-- where N represent delay of circuitry (approx total number of registers)
getResponse :: (Analyzer ZipList SpecWide -> Analyzer ZipList SpecWide) -- ^ circuitry
            -> Double  -- ^ phase in radians that represents Nyquist frequency. (phase pi ≡ 1.0 of sampling frequency)
            -> [Spectrum Rational Double] -- ^ response as stream
getResponse f ph = getZipList $ snd $ unAnalyzer $ f $ Analyzer ([ Just (delaySpec ph)] , pure (constSpectrum 1 1))

-- | run analysis for both acyclic\/cyclic and only linear circuits
-- faster version of getResponse for acyclic linear ciruits
getResponseLin :: (Analyzer ZipList SpecLin -> Analyzer ZipList SpecLin) -- ^ circuitry
          -> Double -- ^ phase in radians that represents Nyquist frequency. (phase pi ≡ 1.0 of sampling frequency)
          -> [SpecLin] -- ^ response as stream
getResponseLin f ph = getZipList $ snd $ unAnalyzer $ f $ Analyzer ([Just (delayLinSpec ph)] , pure (SpecLin (0,1 :+ 0)))



-- | run analysis both linear\/nonlinear and only acyclic circuit
-- calculate response of acyclic circuit
getResponseAcyclic :: (AnalyzerAcyclic SpecWide -> AnalyzerAcyclic SpecWide) -- ^ circuitry
                      -> Double -- ^ phase in radians that represents Nyquist frequency. (phase pi ≡ 1.0 of sampling frequency)
                      -> Spectrum Rational Double -- ^ response as stream
getResponseAcyclic f ph =  snd $ unAnalyzerAcyclic $ f $ AnalyzerAcyclic (Just (delaySpec ph) ,constSpectrum 1 1)

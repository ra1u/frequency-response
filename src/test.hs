{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}


import FrequencyResponse

import CLaSH.Prelude 
import CLaSH.Signal.MultiSignal
import Data.Complex as C
import qualified Prelude as P
import Data.Maybe
import Control.Applicative
import Graphics.EasyPlot
import Data.Map.Strict as Map

{-
Ciruits and stuff
-}

registerP = prepend

firP coeffs x = dotp coeffs (windowP x)
  where
    dotp as bs = sum (zipWith (*) as bs)

iirP cA cB x = r where
    oB =  firP cB x
    oA =  firP cA (registerP def r)
    r = oB - oA

-- fir coeficients
firCoef :: Fractional a => Vec 201 a
firCoef = -6.86056317e-19 :> 1.08643703e-04 :> -6.04051550e-18 :> -2.03379801e-04 :> -2.01638331e-04 :> 1.96946883e-18 :> 2.90791068e-18 :> -4.62180925e-04 :> -1.00665733e-03 :> -9.14037108e-04 :> 3.86774364e-18 :> 1.01950810e-03 :> 1.25131737e-03 :> 6.39186084e-04 :> -8.00554425e-18 :> 2.62712857e-18 :> 3.77239653e-04 :> 4.17356596e-04 :> -1.21745475e-17 :> -2.64065618e-04 :> -5.23918059e-18 :> 2.99576343e-04 :> -2.73859684e-18 :> -6.08801988e-04 :> -6.23370976e-04 :> 1.63484509e-17 :> -7.03525331e-18 :> -1.52438502e-03 :> -3.36082345e-03 :> -3.07661933e-03 :> -1.30788363e-17 :> 3.45259832e-03 :> 4.23241069e-03 :> 2.15427395e-03 :> 0.00000000e+00 :> 2.70383448e-17 :> 1.24466846e-03 :> 1.36380726e-03 :> -2.92823518e-17 :> -8.44354542e-04 :> -4.24392593e-17 :> 9.35427798e-04 :> 2.64177628e-17 :> -1.85473668e-03 :> -1.87576073e-03 :> 2.24314248e-18 :> -5.19238250e-17 :> -4.42290170e-03 :> -9.63775624e-03 :> -8.72271164e-03 :> 3.66725201e-17 :> 9.57794343e-03 :> 1.16209519e-02 :> 5.85688426e-03 :> -6.95248052e-17 :> 1.32593176e-17 :> 3.29422893e-03 :> 3.58089966e-03 :> 9.08965047e-18 :> -2.18539647e-03 :> -2.12919221e-17 :> 2.39194534e-03 :> -8.31327663e-17 :> -4.69672484e-03 :> -4.73138591e-03 :> 5.79766610e-17 :> -5.28939995e-18 :> -1.10712423e-02 :> -2.40989091e-02 :> -2.18047819e-02 :> -6.11496326e-17 :> 2.39909500e-02 :> 2.91782046e-02 :> 1.47558239e-02 :> 3.11706634e-17 :> 1.33673427e-18 :> 8.44218666e-03 :> 9.25308362e-03 :> -9.62845645e-17 :> -5.76785543e-03 :> -2.84709264e-17 :> 6.49485159e-03 :> -4.19102125e-17 :> -1.32387105e-02 :> -1.36437840e-02 :> 1.16146829e-16 :> -4.98075231e-17 :> -3.49290893e-02 :> -7.90710907e-02 :> -7.48583001e-02 :> -3.01478553e-33 :> 9.23333495e-02 :> 1.20794568e-01 :> 6.66720749e-02 :> -9.64385396e-17 :> 4.65544367e-17 :> 5.76578486e-02 :> 7.99902785e-02 :> -8.32650050e-17 :> -1.34035769e-01 :> 8.00710266e-01 :> -1.34035769e-01 :> -8.32650050e-17 :> 7.99902785e-02 :> 5.76578486e-02 :> 4.65544367e-17 :> -9.64385396e-17 :> 6.66720749e-02 :> 1.20794568e-01 :> 9.23333495e-02 :> -3.01478553e-33 :> -7.48583001e-02 :> -7.90710907e-02 :> -3.49290893e-02 :> -4.98075231e-17 :> 1.16146829e-16 :> -1.36437840e-02 :> -1.32387105e-02 :> -4.19102125e-17 :> 6.49485159e-03 :> -2.84709264e-17 :> -5.76785543e-03 :> -9.62845645e-17 :> 9.25308362e-03 :> 8.44218666e-03 :> 1.33673427e-18 :> 3.11706634e-17 :> 1.47558239e-02 :> 2.91782046e-02 :> 2.39909500e-02 :> -6.11496326e-17 :> -2.18047819e-02 :> -2.40989091e-02 :> -1.10712423e-02 :> -5.28939995e-18 :> 5.79766610e-17 :> -4.73138591e-03 :> -4.69672484e-03 :> -8.31327663e-17 :> 2.39194534e-03 :> -2.12919221e-17 :> -2.18539647e-03 :> 9.08965047e-18 :> 3.58089966e-03 :> 3.29422893e-03 :> 1.32593176e-17 :> -6.95248052e-17 :> 5.85688426e-03 :> 1.16209519e-02 :> 9.57794343e-03 :> 3.66725201e-17 :> -8.72271164e-03 :> -9.63775624e-03 :> -4.42290170e-03 :> -5.19238250e-17 :> 2.24314248e-18 :> -1.87576073e-03 :> -1.85473668e-03 :> 2.64177628e-17 :> 9.35427798e-04 :> -4.24392593e-17 :> -8.44354542e-04 :> -2.92823518e-17 :> 1.36380726e-03 :> 1.24466846e-03 :> 2.70383448e-17 :> 0.00000000e+00 :> 2.15427395e-03 :> 4.23241069e-03 :> 3.45259832e-03 :> -1.30788363e-17 :> -3.07661933e-03 :> -3.36082345e-03 :> -1.52438502e-03 :> -7.03525331e-18 :> 1.63484509e-17 :> -6.23370976e-04 :> -6.08801988e-04 :> -2.73859684e-18 :> 2.99576343e-04 :> -5.23918059e-18 :> -2.64065618e-04 :> -1.21745475e-17 :> 4.17356596e-04 :> 3.77239653e-04 :> 2.62712857e-18 :> -8.00554425e-18 :> 6.39186084e-04 :> 1.25131737e-03 :> 1.01950810e-03 :> 3.86774364e-18 :> -9.14037108e-04 :> -1.00665733e-03 :> -4.62180925e-04 :> 2.90791068e-18 :> 1.96946883e-18 :> -2.01638331e-04 :> -2.03379801e-04 :> -6.04051550e-18 :> 1.08643703e-04 :> -6.86056317e-19 :> Nil

-- iir coeficients
iirCoefA :: Fractional a => Vec 8 a
iirCoefB :: Fractional a => Vec 9 a
iirCoefA = -1.8933239813032006 :> 3.422153467072297 :> -3.907588921469733 :> 3.6401868030176012 :> -2.5428541496659465 :> 1.3470500696022936 :> -0.49424202583584054 :> 0.10443313376764539 :> Nil
iirCoefB = 0.0025798085212212327 :> 0.02063846816976986 :> 0.07223463859419452 :> 0.14446927718838903 :> 0.18058659648548628 :> 0.14446927718838903 :> 0.07223463859419452 :> 0.02063846816976986 :> 0.0025798085212212327 :> Nil

{-
Drawing helpers
-}

simFuncLin f skip ph =  snd $ getSpecLin <$> P.last $ P.take skip $ getResponseLin f ph
simFunc f skip ph =  (Map.! 1) $ getSpectrum <$> P.last $ P.take skip $ getResponse f ph
simFuncAcyclic f ph =  (Map.! 1) $ getSpectrum $ getResponseAcyclic f ph


genData f sk pn = (x,y) where
    x = fmap (\a -> (1.0 * pi * fromInteger a / fromInteger pn)) [0, 1 .. pn]
    y = fmap (simFunc f sk) x 

-- how is mapping over result like Complex Double -> Double
-- f is ciruit
-- sk skip first sk values from result, for acyclic circuit this should be more than circuitry delay
-- pn - number of linear points from 0 to 1 (nyquist freq)
plt how f sk pn = plot' [] X11 $ Data2D [Style Lines,Title "plot"] [] (P.zip ((/pi) <$> x) (how <$> y))
  where
    (x,y) = genData f sk pn


genDataLin f sk pn = (x,y) where
    x = fmap (\a -> (1.0 * pi * fromInteger a / fromInteger pn)) [0, 1 .. pn]
    y = fmap (simFuncLin f sk) x 

pltLin how f sk pn = plot' [] X11 $ Data2D [Style Lines,Title "plot"] [] (P.zip ((/pi) <$> x) (how <$> y))
  where
    (x,y) = genDataLin f sk pn


genDataAcyclic f pn = (x,y) where
    x = fmap (\a -> (1.0 * pi * fromInteger a / fromInteger pn)) [0, 1 .. pn]
    y = fmap (simFuncAcyclic f) x 


pltDataAcyclic how f pn = plot' [] X11 $ Data2D [Style Lines,Title "plot"] [] (P.zip ((/pi) <$> x) (how <$> y))
  where
    (x,y) = genDataAcyclic f pn




magLog x = logBase 10 (magnitude x) * 20
magLogMin m x = max m (magLog x)

integrator i = r where
    r = (registerP 0 r) + i

{-
Examples
-}

-- plots working with linear circuits
plotFirLin = pltLin magnitude f 210 100 where
 f = firP firCoef


-- slower and generic version of samplePlotLin
plotFir = plt magnitude f 210 100 where
 f = firP firCoef


-- plot acyclic ciruit
plotFirAcyclic = pltDataAcyclic magnitude f 1000 where
 f = firP firCoef

plotIir = plt magnitude (iirP iirCoefA iirCoefB) 200 100
plotIirLog = pltLin magLog (iirP iirCoefA iirCoefB) 200 100


plotInteg = plt magnitude integrator 100 100

main = plotIir

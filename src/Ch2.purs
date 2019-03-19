module Ch2 where 

import Prelude
import Effect (Effect)
import Effect.Console (logShow)
import Math (sqrt, pi)

main :: Effect Unit
main = do
  logShow $ diagonal 3.0 4.0
  logShow $ circleArea 5.0

diagonal :: Number -> Number -> Number
diagonal w h = sqrt $ w * w + h * h

circleArea :: Number -> Number
circleArea r =
    r * r * pi

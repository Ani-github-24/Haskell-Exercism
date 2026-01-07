module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = 
  let earthyears = seconds / (365.25 * 24 * 60 * 60)
  in case planet of 
    Mercury -> earthyears / 0.2408467
    Venus ->  earthyears / 0.61519726
    Earth -> earthyears / 1.0
    Mars -> earthyears / 1.8808158
    Jupiter -> earthyears / 11.862615
    Saturn -> earthyears / 29.447498
    Uranus -> earthyears / 84.016846
    Neptune -> earthyears / 164.73132
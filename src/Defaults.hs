module Defaults where

import Types

-- | A default set of bikes
defaultBikes :: [Motorcycle]
defaultBikes = [Motorcycle {motorcycleName = "Crossbones", thresholds = [815, 1266]}, 
                Motorcycle {motorcycleName = "Sporty", thresholds=[2862, 3140]}]
               

defaultConfiguration :: HJConfig
defaultConfiguration = HJConfig {
                         teensy = "/dev/cu.usbmodem12341",
                         mic    = "hw:0,0",
                         -- these are the default thresholds, tuned from the crossbones and sporty
                         bikes = defaultBikes,
                         hitsRequired = 6,
                         activationThreshold = 0.01
                       }

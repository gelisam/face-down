module FaceTrace.Types where

-- offset from top-left
type Pos = (Int, Int)

-- gloss units from center, +X is right and +Y is up.
type Coord = (Float, Float)


type Seconds = Double

-- seconds since the beginning of the movie
type Timestamp = Seconds

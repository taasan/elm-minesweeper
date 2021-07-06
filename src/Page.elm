module Page exposing (LevelStep(..), Modal(..))


type LevelStep
    = Topology
    | GridType
    | Actor
    | Dimensions


type Modal
    = LevelChooser LevelStep

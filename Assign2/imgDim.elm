port module ImgDim exposing(..)

import Html exposing (..)
import Html.Attributes exposing (..)


--code copied from https://stackoverflow.com/questions/48551782/elm-get-the-size-of-an-image
-- this port passes our string out of Elm and into
-- js land
port getDim : String -> Cmd msg
-- this port handles our incomming height and width
-- and passes it to a Msg constructor
port newDim : (( Int, Int ) -> msg) -> Sub msg
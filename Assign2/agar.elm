module Main exposing (..)

import Html exposing (..)
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Keyboard as Key
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Random exposing (..)
import ImgDim exposing (..)

--a highly simplified version of agar.io, not going to bother with the time needed to make the background scroll and other stuff

--to make the type definitions a lot more easier
type alias FeedBit = List Feed
type alias Feed = {x:Int,y:Int,value:Int,color:String}

type Either a b = RS a | LS b -- lazy implementation of Haskell's Either

type alias Display = Either PImage String -- can be one or the other, need to extract on display
type alias PImage = {source:String, x: Int, y:Int} -- basic stuff needed to model the player image

type alias Model = {x:Int, y:Int,
                    feed: FeedBit,size:Float,display:Display} -- list of records representing the different dots etc

type Msg = KeyMsg Key.KeyCode | RandResult (Int,Int) 


init : (Model, Cmd.Cmd Msg)
init = ({x=round<| (toFloat svWidth)/2,y=round <| (toFloat svHeight)/2,feed=[],size=25,display = RS {x = 512,y=512, source = "https://images-na.ssl-images-amazon.com/images/I/51zLZbEVSTL._SY355_.jpg"}}, Cmd.none)


extractMod : (Model, Cmd.Cmd Msg) -> Model
extractMod (model,_)=model

--resets the game model
resetGame : Model -> Model
resetGame model = let 
        initial = extractMod init

    in {model | x=initial.x, y = initial.y, feed = initial.feed, size = initial.size}



incNum : Int
incNum = 10
--takes boundary, position
boundsCheck : Int -> Int ->Int -> Int
boundsCheck bounds pos rad = if pos>=bounds+rad then
                            pos-bounds-(2*rad)
                        else if pos <= -rad then
                            pos + bounds+(2*rad)
                        else
                            pos
svWidth : Int 
svWidth = 1300

svHeight : Int
svHeight = 600

--compute the radius based on the size
mr :Int -> Int 
mr i = round <| Basics.sqrt <| ((toFloat i)/Basics.pi)*2

--legacy stuff
radius : Int
radius = 10

--wrapping
bCheckx : Int -> Int ->Int
bCheckx pos size = boundsCheck svWidth pos (mr size)

bChecky : Int ->Int -> Int
bChecky pos size = boundsCheck svHeight pos (mr size)

--render feed bits
buildFeeds: FeedBit -> List (Svg.Svg msg)
buildFeeds feed = case feed of -- literally just a bunch of tiny circles
                    (f::fs) -> (Svg.circle [cx (toString f.x),cy (toString f.y), r (toString <| mr f.value), fill f.color][])::(buildFeeds fs)
                    []      -> []-- return empty list on list end

--update model consuming feeds that overlap with circle
testConsume: Model -> Model
testConsume model = let
        feeds = model.feed
        consumed = List.map (canConsume (mr <| round <| model.size) model.x model.y) feeds -- messy mapping stuff to produce a boolean list
        both = wrap consumed feeds --wrap the two together lazily in a tuple
    in {model | feed = filterOut both [], size = shrink <|consume both model.size}--update model

shrink:Float -> Float
--shrink size = size - (sqrt ((size-25) /10000000) ) -- basically shrink faster if larger, though at a slower rate, pretty much grow forever like this
shrink size = size - (((size*size)-625) / 10000000) --shrink really fast when big, constant gen max ~ 2236 (can get higher if you game it), rng god gen max about 70711

--lazy wrapping cause seriously no way to do this kind of thing without going into a bunch of messy case x of and Maybes
--Also the two lists should be the same size in the usage scenario
wrap : List a -> List b -> List (a,b)
wrap a b = case a of
        (n::ns) -> case b of 
                       (b::bs) -> (n,b)::(wrap ns bs)
                       []      -> []
        []      -> []

filterOut : List (Bool,Feed) -> List Feed -> List Feed
filterOut both feeds = case both of 
        ((True,_)::fs)  -> filterOut fs feeds -- consumed skip that feed item
        ((False,f)::fs) -> filterOut fs (f::feeds) -- merge 
        []              -> feeds
--list of whether can consumeor not > feed list to extract size > current size
consume: List (Bool,Feed) -> Float -> Float 
consume feeds size = case feeds of
        ((True,f)::fs)  -> consume fs (size+(toFloat <| f.value))
        ((False,_)::fs) -> consume fs size
        []              -> size

-- ints are radius and position
canConsume: Int -> Int -> Int -> Feed -> Bool
canConsume r x y f= let
            distance = Basics.sqrt (toFloat (((f.x-x)*(f.x-x))+((f.y-y)*(f.y-y))))
        in (distance<(toFloat <| r+ (mr f.value)))


genFeed: Int -> Int -> Model -> Model
genFeed a b model = case a of 
            1 -> {model | feed = addFeed model.feed 5 (b%svWidth) (round <| toFloat b/(toFloat svWidth))} 
            _ -> model -- only gen on rand = 1
addFeed: FeedBit -> Int -> Int -> Int -> FeedBit
addFeed f v x y = {x=x, y=y,value=v,color=genColor (x+y)}::f

--semi random based on position
genColor : Int -> String
genColor x = let 
            y= x%14 --change to suit colors
        in 
            case y of 
                0 -> "red"
                1 -> "blue"
                2 -> "green"
                3 -> "yellow"
                4 -> "purple"
                5 -> "magenta"
                6 -> "orange"
                7 -> "lime"
                8 -> "cyan"
                9 -> "black"
                10 -> "teal"
                11 -> "grey"
                12 -> "brown"
                _ -> "white" -- the devil's dot, aka it can't be seen

update : Msg -> Model -> (Model, Cmd.Cmd Msg)
update msg model = case msg of --wasd
            (KeyMsg 87) -> (testConsume {model|y = bChecky (model.y-incNum) (round <| model.size)},genRand)
            (KeyMsg 65) -> (testConsume {model|x = bCheckx (model.x-incNum) (round <| model.size)},genRand)
            (KeyMsg 83) -> (testConsume {model|y = bChecky (model.y+incNum) (round <| model.size)},genRand)
            (KeyMsg 68) -> (testConsume {model|x = bCheckx (model.x+incNum) (round <| model.size)},genRand)
            --arrow keys
            (KeyMsg 38) -> (testConsume {model|y = bChecky (model.y-incNum) (round <| model.size)},genRand)
            (KeyMsg 37) -> (testConsume {model|x = bCheckx (model.x-incNum) (round <| model.size)},genRand)
            (KeyMsg 40) -> (testConsume {model|y = bChecky (model.y+incNum) (round <| model.size)},genRand)
            (KeyMsg 39) -> (testConsume {model|x = bCheckx (model.x+incNum) (round <| model.size)},genRand)
            --reset the game if escape key is pressed
            (KeyMsg 27) -> (resetGame model, Cmd.none)
            (RandResult (a,b)) -> if (List.length model.feed < 100)  --limit so there's an upper bound to the size
                                then (genFeed a b model,Cmd.none) 
                                else (model,Cmd.none)
            _ -> (model,Cmd.none)


genRand = generate RandResult (Random.pair (int 1 10 ) (int 1 (svWidth*svHeight)))

view : Model -> Html Msg
view model =
    let 
        posX = (toString model.x)
        posY = (toString model.y)
        feeds = buildFeeds model.feed
        pfill = case model.display of 
            (LS c) -> c
            (RS _) -> "url(#player)" -- use the pattern image
        pImage = case model.display of
            (LS _) -> image [][]
            (RS r) -> image [x "0%",y "0%",height (toString r.y),width (toString r.x),Svg.Attributes.xlinkHref r.source][]
        vBox = case model.display of
            (LS _) -> viewBox "0 0 512 512"
            (RS r) -> viewBox ("0 0 "++(r.x |>toString)++" "++(r.y |> toString))

    in div[][
        svg[Svg.Attributes.width (toString svWidth),Svg.Attributes.height (toString svHeight)](
            [--image in svg based on this https://stackoverflow.com/questions/29442833/svg-image-inside-circle
            defs [][
                pattern[id "player", x "0%", y "0%", height "100%", width "100%", vBox][
                    pImage
                    ]
                ]
            ]++feeds++[Svg.circle [cx posX,cy posY, r (toString <| mr <| round model.size),fill pfill, stroke "black", Svg.Attributes.strokeWidth "1px"] []])
        ,div[][Html.text ("Score "++(toString <|round<| model.size-25))]
        ,div[][Html.text "To play use the arrow keys to move the red circle around. The goal is to consume the small dots and grow as much as possible, if you get over 2236 you win!"]
        ]

subscriptions : Model -> Sub.Sub Msg
subscriptions model = Key.downs KeyMsg

main : Program Never Model Msg
main = program
        {init = init,
        view = view,
        update = update,
        subscriptions = subscriptions}
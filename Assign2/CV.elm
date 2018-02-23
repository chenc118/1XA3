module Main exposing (..)
import Html exposing (..)


main = div [][
    header [][
        h1 [][ text "CV Page" ]
        ]
    ,section [][
        h3 [][ text "Volunteering Experience" ]
        ,ol[][
            li[][
                h5 [] [text "Volunteering Experience title 1"]
                ,div [] [ text "Volunteering Experience Description short" ]
                ]
            ,li [][
                h5 [] [ text "Volunteering Experience title 2"]
                ,div [][ text "Volunteering Experience Description short"]
                ]
            ]
        ]
    ,section [][
        h3 [] [text "Workplace Skills" ]
        ,ul[][
            li [] [text  "Skill # 1"]
            ,li [] [text "Skill # 2"]
            ,li [] [text "Skill # 3"]
            ,li [] [text  "Skill # 4"]
            ]
        ]
    ,footer [] [
        h5 [] [text "Copyright @ Name 2018"]
        div [] [ text "email:chenc118@mcmaster.ca"]
        div [] [ text "github:github.com/chenc118"]
        ] 
    ]
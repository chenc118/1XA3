module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)




headerStyle = style [("color","white"),
                     ("padding","0px 0px 0px 0px"),
                     ("margin","0px 0px 0px 0px")]

asdfText: Int -> String
asdfText num = case num of 
                      0 -> "asdf"
                      _ -> "asdf "++(asdfText (num-1))
main = div [][
          header[][
              h1 [style [("text-align","center"),("color","white"),("background-color","dodgerblue"),("margin","0"),("padding","0")]] [text "My Ugly Nonsense Webpage"]
            ]
          ,div [style [("color","white"),("background-color","tomato"),("padding","2em 2em 0em 2em")]][
            section[style [("float","right"),("text","white")]][
              h2[headerStyle][text "Useless List"],
              ol[][
                li [] [text "Link 1"]
                ,li [] [text "Link 2"]
                ,li [] [text "Link 3"]
                ,li [] [text "Link 4"]
                ,li [] [text "Link 5"]
                ,li [] [text "Link 6"]
                ,li [] [text "Link 7"]
                ,li [] [text "Link 8"]
                ,li [] [text "Link 9"]
                ,li [] [text "Link 10"]
                ]
              ]
            ,section [][
              h2[headerStyle][text "First Section of Nonsense"]
              ,p [] [text (asdfText 324)]
             ]
            ,section[][
              h2 [headerStyle] [text "Second Section of More Nonsense"]
              ,p[][
                div [style [("text-align","center"),("padding","0"),("margin","0"),("float","left"),("clear","left")]][
                  div[] [text "fat man does small door"]
                  ,div[][text "he knows he cannot fit through"]
                  ,div[][text "tears flow free now"]
                  ]
                ]
             ]
            ,footer[style [("clear","left "),("margin","0"),("padding","1em 0 0 0")]][
              div[style [("text-align","center")]][strong [] [text "Copyright NonsenseCompany @ 2018"]]
             ]
            ]
          ]
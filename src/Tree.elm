module Tree exposing (..)


type alias TreeItem =
    { id : String
    , title : String
    , isVisible : Bool
    , level : Int
    , children : Maybe Children
    }


type Children
    = Children (List TreeItem)


getChildren subs =
    case subs of
        Children items ->
            items


mapAllNodes : (TreeItem -> TreeItem) -> List TreeItem -> List TreeItem
mapAllNodes mapper items =
    List.map (mapItem mapper) items


mapItem : (TreeItem -> TreeItem) -> TreeItem -> TreeItem
mapItem mapper item =
    case item.children of
        Just someChildren ->
            mapper { item | children = children (mapAllNodes mapper (getChildren someChildren)) }

        Nothing ->
            mapper item


getParents : List TreeItem -> String -> List TreeItem
getParents model itemId =
    getParentsImp model itemId []


getParentsImp nodes itemId parents =
    case find (hasChild itemId) nodes of
        Just parent ->
            getParentsImp nodes parent.id (List.append [ parent ] parents)

        Nothing ->
            parents


hasChild : String -> TreeItem -> Bool
hasChild id node =
    case node.children of
        Just subs ->
            List.any (\sub -> sub.id == id) (getChildren subs)

        Nothing ->
            False


getNodesFlattenedWithLevels : List TreeItem -> List TreeItem
getNodesFlattenedWithLevels nodes =
    List.map (flattenChildren 0) nodes |> List.concat


flattenChildren : Int -> TreeItem -> List TreeItem
flattenChildren level n =
    case ( n.children, n.isVisible ) of
        ( Just subs, True ) ->
            List.append [ { n | level = level } ] (List.concat (List.map (flattenChildren (level + 1)) (getChildren subs)))

        _ ->
            [ { n | level = level } ]


find : (TreeItem -> Bool) -> List TreeItem -> Maybe TreeItem
find predicate items =
    let
        select : TreeItem -> List TreeItem
        select node =
            case node.children of
                Just subs ->
                    List.concat [ [ node ], List.concat (List.map select (getChildren subs)) ]

                Nothing ->
                    [ node ]

        flatNodes =
            List.map select items |> List.concat
    in
    List.filter predicate flatNodes |> List.head


initialNodes : List TreeItem
initialNodes =
    [ { title = "Trance"
      , id = "tranceNode"
      , isVisible = True
      , level = 0
      , children =
            children
                [ { title = "Deep house"
                  , id = "deepHouseNode"
                  , isVisible = True
                  , level = 0
                  , children =
                        children
                            [ leafItem "6b71fb1aeeaa0db208ef3f7e" "Deep House Mix 2015 #92 | Tropical House Mix by Luca dot DJ"
                            , leafItem "59ee76602effda59422b1c26" "The Best Of Vocal Deep House Nu Disco 2013 (2 Hour Mixed By Zeni N)"
                            , leafItem "247ca74070c99d7c941f64c5" "Deep House Mix 2015 #85 | New House Music Mixed by XYPO"
                            , leafItem "8e06b8a6cae789977ead9309" "Feeling Happy - Best Of Vocal Deep House Music Chill Out - Mix By Regard #3"
                            , leafItem "979b8fca092882548968cedd" "The Best Of Vocal Deep House Chill Out Music 2015 (2 Hour Mixed By Regard ) #1"
                            , leafItem "1ef218d1a09cb57c8af0d370" "Best of Vocal Trance Mix HD"
                            , leafItem "149ec02dab0497ccad35b73b" "Deep & Elegant Vibes • Deep House Mix [Gentleman Selection Vol.4]"
                            , leafItem "661422b5f047f7ae37278e2a" "Y3lloW - Deep House Vocals Winter 2014 Vol.1 HQ"
                            , leafItem "c28910e52f36c5272e4dfb9e" "Deep House Mix 2015 #85 | New House Music Mixed by XYPO"
                            , leafItem "2038c5413d5fd3a8926defe3" "DEEP HOUSE MIX 2015 (1Hour) | Chilled Deep | Vol. 2"
                            , leafItem "f49a66b88ef98cbd9789554c" "Best Deep House Vocal Mix 2015 (1Hour) | Vol. 5"
                            , leafItem "704ac83a21261b906a8208fd" "Deep House Mix 2015 #75 | New Music Mixed by Melody4emotion"
                            , leafItem "197ea501c4264a23c07b84f4" "Deep House Chill Out Lounge Music | Mixed By Dj Regard | 2013 |"
                            , leafItem "55a3af3327dea10d6862c7b6" "♫ Best Deep House Mix 2018 Vol. #1 ♫"
                            , leafItem "a66e1857726b97f95b759941" "The Best Of Vocal Deep House Chill Out Music 2015 (2 Hour Mixed By Regard ) #4"
                            , leafItem "4e2dd8c466dc30854f6d41a5" "Deep House Mix 2015 #104 | New Music Mixed by XYPO"
                            , leafItem "6fbad7832e79ee73741e21b8" "Progressive House · Relaxing Focus Music · 24/7 Live Radio"
                            , leafItem "0e7577962d8ad5c77e74c5b3" "Ibiza Vibes | Deep House Mix &amp; Tech House Music 2016 #145 by XYPO"
                            ]
                  }
                , { title = "Boris Brejcha"
                  , isVisible = True
                  , level = 0
                  , id = "borisNode"
                  , children =
                        children
                            [ leafItem "dd869e870550f28308e13ca5" "Boris Brejcha @ Art of Minimal Techno Tripping - The Mad Doctor by RTTWLR"
                            , leafItem "f28cfb99ddc1cea9c595b73e" "Boris Brejcha @ Art of Minimal Techno Tripping - Mickey &amp; Bad Hot Dogs by RTTWLR"
                            , leafItem "5f2a81e164b6b2ee580243fb" "Boris Brejcha & Trippy Code @ Art of Minimal Techno & Melodic Good Life Radio 24/7 Live"
                            , leafItem "264454440520c45706a7b17f" "Boris Brejcha &amp; Art of Minimal Techno Favourites - Classic Cartoons by RTTWLR"
                            , leafItem "5f344ec3707a56542478b657" "Boris Brejcha @ Art of Minimal Techno Tripping - Lucky Rabbit by RTTWLR"
                            ]
                  }
                ]
      }
    , { title = "Ambient", isVisible = True, id = "ambientNode", level = 0, children = children [] }
    ]


children items =
    Maybe.Just (Children items)


leafItem : String -> String -> TreeItem
leafItem id title =
    { id = id, title = title, isVisible = True, children = Maybe.Nothing, level = 0 }

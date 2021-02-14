module Collection exposing (addToGroup, createOrAppend, groupBy)

import Dict exposing (Dict)


groupBy : (a -> comparable) -> List a -> Dict comparable (List a)
groupBy identify items =
    items
        |> List.foldl (addToGroup identify) Dict.empty


addToGroup : (a -> comparable) -> a -> Dict comparable (List a) -> Dict comparable (List a)
addToGroup identify item groups =
    Dict.update (identify item) (createOrAppend item) groups


createOrAppend : a -> Maybe (List a) -> Maybe (List a)
createOrAppend item previous =
    case previous of
        Nothing ->
            Just [ item ]

        Just list ->
            Just (item :: list)

module Generated.Rankings.Route exposing
    ( Route(..)
    , toPath
    )

import Generated.Rankings.Params as Params


type Route
    = Dynamic String Params.Dynamic
    | Top Params.Top


toPath : Route -> String
toPath route =
    case route of
        Top _ ->
            "/"
        
        
        Dynamic value _ ->
            "/" ++ value
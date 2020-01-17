module Generated.Rankings.Route exposing
    ( Route(..)
    , toPath
    )

import Generated.Rankings.Params as Params
import Generated.Docs.Dynamic.Route


type Route
    = Dynamic String Params.Dynamic
    | Top Params.Top
    | Dynamic_Folder String Generated.Docs.Dynamic.Route.Route


toPath : Route -> String
toPath route =
    case route of
        Top _ ->
            "/"
        
        
        Dynamic value _ ->
            "/" ++ value
        
        
        Dynamic_Folder value subRoute ->
            "/" ++ value ++ Generated.Docs.Dynamic.Route.toPath subRoute
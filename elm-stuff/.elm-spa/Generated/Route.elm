module Generated.Route exposing
    ( Route(..)
    , toPath
    )

import Generated.Params as Params
import Generated.Docs.Route


type Route
    = Guide Params.Guide
    | NotFound Params.NotFound
    | Top Params.Top
    | Docs_Folder Generated.Docs.Route.Route


toPath : Route -> String
toPath route =
    case route of
        Guide _ ->
            "/guide"
        
        
        NotFound _ ->
            "/not-found"
        
        
        Top _ ->
            "/"
        
        
        Docs_Folder subRoute ->
            "/docs" ++ Generated.Docs.Route.toPath subRoute
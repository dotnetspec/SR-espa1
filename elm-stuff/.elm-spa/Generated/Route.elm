module Generated.Route exposing
    ( Route(..)
    , toPath
    )

import Generated.Params as Params
import Generated.Docs.Route
import Generated.Rankings.Route


type Route
    = AboutUs Params.AboutUs
    | Guide Params.Guide
    | NotFound Params.NotFound
    | Top Params.Top
    | Docs_Folder Generated.Docs.Route.Route
    | Rankings_Folder Generated.Rankings.Route.Route


toPath : Route -> String
toPath route =
    case route of
        AboutUs _ ->
            "/about-us"
        
        
        Guide _ ->
            "/guide"
        
        
        NotFound _ ->
            "/not-found"
        
        
        Top _ ->
            "/"
        
        
        Docs_Folder subRoute ->
            "/docs" ++ Generated.Docs.Route.toPath subRoute
        
        
        Rankings_Folder subRoute ->
            "/rankings" ++ Generated.Rankings.Route.toPath subRoute
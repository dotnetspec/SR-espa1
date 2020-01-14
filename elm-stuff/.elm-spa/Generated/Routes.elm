module Generated.Routes exposing
    ( Route
    , parsers
    , routes
    , toPath
    )

import Generated.Route
import Generated.Docs.Route
import Generated.Docs.Dynamic.Route
import Url.Parser as Parser exposing ((</>), Parser, map, s, string, top)



-- ALIASES


type alias Route =
    Generated.Route.Route


toPath : Route -> String
toPath =
    Generated.Route.toPath



-- ROUTES


type alias Routes =
    { guide : Route
    , notFound : Route
    , top : Route
    , docs_top : Route
    , docs_dynamic : String -> Route
    , docs_dynamic_dynamic : String -> String -> Route
    }


routes : Routes
routes =
    { guide =
        Generated.Route.Guide {}
    , notFound =
        Generated.Route.NotFound {}
    , top =
        Generated.Route.Top {}
    , docs_top =
        Generated.Route.Docs_Folder <|
            Generated.Docs.Route.Top {}
    , docs_dynamic =
        \param1 ->
            Generated.Route.Docs_Folder <|
                Generated.Docs.Route.Dynamic param1 { param1 = param1 }
    , docs_dynamic_dynamic =
        \param1 param2 ->
            Generated.Route.Docs_Folder <|
                Generated.Docs.Route.Dynamic_Folder param1 <|
                    Generated.Docs.Dynamic.Route.Dynamic param2 { param1 = param1, param2 = param2 }
    }
 

parsers : List (Parser (Route -> a) a)
parsers =
    [ map routes.guide
        (s "guide")
    , map routes.notFound
        (s "not-found")
    , map routes.top
        (top)
    , map routes.docs_top
        (s "docs" </> top)
    , map routes.docs_dynamic
        (s "docs" </> string)
    , map routes.docs_dynamic_dynamic
        (s "docs" </> string </> string)
    ]
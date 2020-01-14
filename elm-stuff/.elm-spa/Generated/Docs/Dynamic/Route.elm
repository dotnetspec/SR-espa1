module Generated.Docs.Dynamic.Route exposing
    ( Route(..)
    , toPath
    )

import Generated.Docs.Dynamic.Params as Params


type Route
    = Dynamic String Params.Dynamic


toPath : Route -> String
toPath route =
    case route of
        Dynamic value _ ->
            "/" ++ value
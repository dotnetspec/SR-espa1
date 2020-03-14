module SR.Constants exposing
    (  globalJsonbinRankingReadLink
       --, globalJsonbinRankingUpdateLink

    , jsonbinUrlForCreateNewRankingAndReturnNewId
    )


globalJsonbinRankingReadLink : String
globalJsonbinRankingReadLink =
    "https://api.jsonbin.io/b/5e66ec74a030db370e1b23fc/latest"



-- globalJsonbinRankingLink : String
-- globalJsonbinRankingLink =
--     "https://api.jsonbin.io/b/5e2a585f593fd741856f4b04/latest"


jsonbinUrlForCreateNewRankingAndReturnNewId : String
jsonbinUrlForCreateNewRankingAndReturnNewId =
    "https://api.jsonbin.io/b/"

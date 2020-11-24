module SR.Constants exposing
    ( globalJsonbinRankingReadLink
    , globalJsonbinRankingUpdateLink
    , jsonbinUrlForCreateNewBinAndRespond
    , jsonbinUrlStubForUpdateExistingBinAndRespond
    , jsonbinUrlUpdateUserListAndRespond
    , jsonbinUsersReadBinLink
    , baseBinUrl
    , endpointURL
    , customKeyBearerToken
    )

endpointURL : String
endpointURL =
    "https://graphql.fauna.com/graphql"

customKeyBearerToken : String
customKeyBearerToken =
    -- Test Server Key
    "Bearer fnAD7T98VfACBeFno5RAUCCOMn8oSlNq03CUNTDc"
    -- Test server suggested:
    --"Bearer fnAD5UBKwIACCOQKx_X7D7S9eCTPDHaCvWWS9d-0"

baseBinUrl : String 
baseBinUrl = 
    "https://api.jsonbin.io/b/"


globalJsonbinRankingReadLink : String
globalJsonbinRankingReadLink =
    "https://api.jsonbin.io/b/5e66ec74a030db370e1b23fc/latest"


jsonbinUsersReadBinLink : String
jsonbinUsersReadBinLink =
    "https://api.jsonbin.io/b/5e4cf5f54d073155b0dca915/latest"


jsonbinUrlUpdateUserListAndRespond : String
jsonbinUrlUpdateUserListAndRespond =
    "https://api.jsonbin.io/b/5e4cf5f54d073155b0dca915/"



jsonbinUrlForCreateNewBinAndRespond : String
jsonbinUrlForCreateNewBinAndRespond =
    "https://api.jsonbin.io/b/"


jsonbinUrlStubForUpdateExistingBinAndRespond : String
jsonbinUrlStubForUpdateExistingBinAndRespond =
    "https://api.jsonbin.io/b/"


globalJsonbinRankingUpdateLink : String
globalJsonbinRankingUpdateLink =
    "https://api.jsonbin.io/b/5e66ec74a030db370e1b23fc/"

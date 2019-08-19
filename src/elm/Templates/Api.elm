module Templates.Api exposing (..)

import ElmDSL
    exposing
        ( Declaration
        , File
        , Import
        , Module
        , application
        , file
        , functionDeclaration
        , functionOrValue
        , functionTypeAnnotation
        , import_
        , literal
        , normalModule
        , signature
        , typed
        , unit
        , varPattern
        )


type alias GenModel =
    { name : List String
    , isRegional : Bool
    , endpointPrefix : String
    , apiVersion : String
    , protocol : String
    , signer : String
    , docs : String
    , imports : List ()
    , operations : List ()
    , types_ : List ()
    }


example : GenModel
example =
    { name = [ "Some", "Module" ]
    , isRegional = True
    , endpointPrefix = "dynamodb"
    , apiVersion = "2012-08-10"
    , protocol = "json"
    , signer = "signV4"
    , docs = ""
    , imports = []
    , operations = []
    , types_ = []
    }


serviceFile : GenModel -> File
serviceFile model =
    let
        moduleSpec =
            module_ model

        ( functions, fullImports ) =
            List.unzip
                [ service model ]

        deDupedImports =
            List.concat fullImports
    in
    file moduleSpec deDupedImports functions []


coreServiceMod : List String
coreServiceMod =
    [ "AWS", "Core", "Service" ]



--== Module Specification (with exposing).
-- module AWS.{{= it.mod }}
--     exposing
--         ( service
--         , {{= it.operationNames.join('\n        , ')}}
--         , {{= it.types.filter(t => t.exposeAs).map(t => t.exposeAs).join('\n        , ')}}
--         )


module_ : GenModel -> Module
module_ model =
    normalModule model.name []



--== Module Documentation
-- {-| {{= it.documentation }}
--
-- @docs service
--
-- ## Table of Contents
--
-- * [Operations](#operations){{~ it.categories :c }}
-- * [{{= c.name }}](#{{= c.name.toLowerCase() }}){{~}}
--
-- ## Operations
--
-- {{~ it.operationNames :name }}* [{{= name }}](#{{= name }})
-- {{~}}
--
-- @docs {{= it.operationNames.join(',') }}
-- {{~ it.categories :c }}
-- ## {{= c.name }}
--
-- {{~ c.types.filter(t => t.exposeAs).map(t => t.type) :t }}* [{{= t }}](#{{= t }})
-- {{~}}
--
-- @docs {{= c.types.filter(t => t.exposeAs).map(t => t.type).join(',') }}
-- {{~}}
-- -}


docs =
    ""



--== Service Definition


service : GenModel -> ( Declaration, List Import )
service model =
    if model.isRegional then
        regionalService model

    else
        globalService model


regionalService : GenModel -> ( Declaration, List Import )
regionalService model =
    let
        sig =
            signature "service"
                (functionTypeAnnotation
                    (typed coreServiceMod "Region" [])
                    (typed coreServiceMod "Service" [])
                )

        impl =
            application
                [ functionOrValue coreServiceMod "defineRegional"
                , literal model.endpointPrefix
                , literal model.apiVersion
                , functionOrValue coreServiceMod model.protocol
                , functionOrValue coreServiceMod model.signer
                ]
    in
    ( functionDeclaration
        (Just "{-| Configuration for this service. -}")
        (Just sig)
        "service"
        []
        impl
    , [ import_ coreServiceMod Nothing Nothing ]
    )


globalService : GenModel -> ( Declaration, List Import )
globalService model =
    let
        sig =
            signature "service"
                (typed coreServiceMod "Service" [])

        impl =
            application
                [ functionOrValue coreServiceMod "defineGlobal"
                , literal model.endpointPrefix
                , literal model.apiVersion
                , functionOrValue coreServiceMod model.protocol
                , functionOrValue coreServiceMod model.signer
                ]
    in
    ( functionDeclaration
        (Just "{-| Configuration for this service. -}")
        (Just sig)
        "service"
        []
        impl
    , [ import_ coreServiceMod Nothing Nothing ]
    )



--== Operations
--
-- -- OPERATIONS
--
-- {{= it.operations.join('\n\n') }}
--
-- {{~ it.types.filter(t => t.exposeAs) :t }}
-- {{= t.typeDef }}
--
-- {{? t.decoderDef }}
-- {{= t.decoderDef }}
-- {{?}}
--
-- {{? t.toStringDef }}
-- {{= t.toStringDef }}
-- {{?}}
-- {{~}}
--
--
-- {{~ it.types.filter(t => t.category === 'request') :t }}
-- {{= t.typeDef }}
-- {{~}}
--
--
-- {{~ it.types.filter(t => t.exposeAs || t.category === 'request') :t }}
-- {{? it.metadata.protocol === 'json' && t.jsonEncoderDef }}
-- {{= t.jsonEncoderDef }}
-- {{?}}
--
-- {{? it.metadata.protocol === 'query' && t.queryEncoderDef }}
-- {{= t.queryEncoderDef }}
-- {{?}}
-- {{~}}

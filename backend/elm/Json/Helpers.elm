module Json.Helpers exposing
    ( ObjectEncoding, encodeObject, encodeValue
    , decodeSumObjectWithSingleField, encodeSumObjectWithSingleField
    , decodeSumTwoElemArray, encodeSumTwoElementArray
    , decodeSumTaggedObject, encodeSumTaggedObject
    , decodeSumUnaries
    , decodeSumNullaries
    , decodeSumNullaryOrSingleField
    , required, fnullable, custom
    , decodeMap, encodeMap, jsonEncDict, jsonDecDict, encodeSet, decodeSet, maybeEncode, encodeSumUntagged
    , tuple2, tuple3
    )

{-| This module exposes helper functions for encoding sum types and maps. It was designed
with an eye for compatibility with the `aeson` library from the Haskell world, which explains
why the various functions have such peculiar names.

If you require Haskell interop, please take a look at the [elm-bridge](https://hackage.haskell.org/package/elm-bridge) package that
will make it easy to derive the Elm code alongside the Haskell one.


# The ObjectEncoding type

@docs ObjectEncoding, encodeObject, encodeValue


# Encoding schemes

The following Elm type will be used as an example for the different encoding schemes.

    type Foo
        = Bar Int
        | Baz { a : Int, b : Int }
        | Qux Int Int


## ObjectWithSingleField

    -- {"Bar":5}
    -- {"Baz":{"a":4,"b":8}}
    -- {"Qux":[98,42]}



@docs decodeSumObjectWithSingleField, encodeSumObjectWithSingleField


## TwoElemArray

    -- ["Bar",5]
    -- ["Baz",{"a":4,"b":8}]
    -- ["Qux",[98,42]]



@docs decodeSumTwoElemArray, encodeSumTwoElementArray


## TaggedObject

    -- {"tag":"Bar","content":5}
    -- {"tag":"Baz","a":4,"b":8}
    -- ["tag":"Qux","content":[98,42]}



@docs decodeSumTaggedObject, encodeSumTaggedObject


## Nullary sum types

@docs decodeSumUnaries
@docs decodeSumNullaries

@docs decodeSumNullaryOrSingleField


# Pipeline utils

@docs required, fnullable, custom


# Containers helpers

@docs decodeMap, encodeMap, jsonEncDict, jsonDecDict, encodeSet, decodeSet, maybeEncode, encodeSumUntagged


# Tuple helpers

@docs tuple2, tuple3

-}

-- https://github.com/bartavelle/json-helpers
--
-- Copyright (c) 2018 Simon Marechal. All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

import Dict exposing (Dict)
import Json.Decode exposing (Value, errorToString, field)
import Json.Encode
import Set exposing (Set)
import Tuple exposing (second)


{-| This is an opaque type that is to be used to give hints when using the `TaggedObject` encoding.
-}
type ObjectEncoding
    = EObject (List ( String, Value ))
    | EValue Value


{-| Creates an `ObjectEncoding`, just like the `Json.Encode.object` function.
-}
encodeObject : List ( String, Value ) -> ObjectEncoding
encodeObject =
    EObject


{-| Creates an `ObjectEncoding` from any type of `Value`. You should not use this for `Value`s that are actually objects.
-}
encodeValue : Value -> ObjectEncoding
encodeValue =
    EValue


oeValue : ObjectEncoding -> Value
oeValue x =
    case x of
        EObject o ->
            Json.Encode.object o

        EValue v ->
            v


{-| Encodes an optional value, using `null` when there is `Nothing`
-}
maybeEncode : (a -> Value) -> Maybe a -> Value
maybeEncode e v =
    case v of
        Nothing ->
            Json.Encode.null

        Just a ->
            e a


resmapM : (a -> Result r b) -> List a -> Result r (List b)
resmapM f lst =
    case lst of
        [] ->
            Ok []

        x :: xs ->
            f x |> Result.andThen (\nx -> resmapM f xs |> Result.andThen (\nxs -> Ok (nx :: nxs)))



-- polyfill


jtuple2 : (a -> b -> value) -> Json.Decode.Decoder a -> Json.Decode.Decoder b -> Json.Decode.Decoder value
jtuple2 abv da db =
    Json.Decode.map2 abv (Json.Decode.index 0 da) (Json.Decode.index 1 db)



-- polyfill from https://groups.google.com/d/msg/elm-dev/Ctl_kSKJuYc/7nCM8XETBwAJ


customDecoder : Json.Decode.Decoder a -> (a -> Result Json.Decode.Error b) -> Json.Decode.Decoder b
customDecoder decoder toResult =
    Json.Decode.andThen
        (\a ->
            case toResult a of
                Ok b ->
                    Json.Decode.succeed b

                Err err ->
                    Json.Decode.fail <| errorToString err
        )
        decoder


{-| Decode objects encoded using the `ObjectWithSingleField` scheme.
The first argument is the human readable name of the type of data, and will be used in error messages.
The second argument is a `Dict` where the keys are the tags of each constructor of the sum type and the values
are decoders for each case.
-}
decodeSumObjectWithSingleField : String -> Dict String (Json.Decode.Decoder a) -> Json.Decode.Decoder a
decodeSumObjectWithSingleField name mapping =
    customDecoder (Json.Decode.keyValuePairs Json.Decode.value)
        (\lst ->
            case lst of
                [] ->
                    Err <| Json.Decode.Failure ("Can't decode " ++ name ++ ": object has too few keys") Json.Encode.null

                [ ( key, value ) ] ->
                    decodeSumFinal name key value mapping

                kv :: kvs ->
                    Err <| Json.Decode.Failure ("Can't decode " ++ name ++ ": object has too many keys") (second kv)
        )


{-| Decode objects encoded using the `TwoElemArray` scheme.
The first argument is the human readable name of the type of data, and will be used in error messages.
The second argument is a `Dict` where the keys are the tags of each constructor of the sum type and the values
are decoders for each case.
-}
decodeSumTwoElemArray : String -> Dict String (Json.Decode.Decoder a) -> Json.Decode.Decoder a
decodeSumTwoElemArray name mapping =
    customDecoder (jtuple2 (\a b -> ( a, b )) Json.Decode.string Json.Decode.value) (\( key, value ) -> decodeSumFinal name key value mapping)


{-| Decode objects encoded using the `TaggedObject` scheme.
The first argument is the human readable name of the type of data, and will be used in error messages.
The second argument is a `Dict` where the keys are the tags of each constructor of the sum type and the values
are decoders for each case.

Compared to the other functions, it expects a set of `String`s. This sets lists all the constructor tags that have an object content,
such as the `Baz` constructor in the example.

-}
decodeSumTaggedObject : String -> String -> String -> Dict String (Json.Decode.Decoder a) -> Set String -> Json.Decode.Decoder a
decodeSumTaggedObject name fieldname contentname mapping objectKeys =
    field fieldname Json.Decode.string
        |> Json.Decode.andThen
            (\key ->
                let
                    decoder =
                        if Set.member key objectKeys then
                            Json.Decode.value

                        else
                            field contentname Json.Decode.value
                in
                customDecoder decoder (\value -> decodeSumFinal name key value mapping)
            )


decodeSumFinal : String -> String -> Value -> Dict String (Json.Decode.Decoder a) -> Result Json.Decode.Error a
decodeSumFinal name key value mapping =
    case Dict.get key mapping of
        Nothing ->
            Err <| Json.Decode.Failure ("Unknown constructor " ++ key ++ " for type " ++ name) value

        Just dec ->
            Json.Decode.decodeValue dec value


{-| Encode objects using the `WithSingleField` scheme.
The first argument is a function that, for each possible value `a`, must return a `String` tag
describing it along with an `ObjectEncoding`.
-}
encodeSumObjectWithSingleField : (a -> ( String, ObjectEncoding )) -> a -> Value
encodeSumObjectWithSingleField mkkeyval v =
    let
        ( key, val ) =
            mkkeyval v
    in
    Json.Encode.object [ ( key, oeValue val ) ]


{-| Encode objects using the `TwoElementArray` scheme.
The first argument is a function that, for each possible value `a`, must return a `String` tag
describing it along with an `ObjectEncoding`.
-}
encodeSumTwoElementArray : (a -> ( String, ObjectEncoding )) -> a -> Value
encodeSumTwoElementArray mkkeyval v =
    let
        ( key, val ) =
            mkkeyval v
    in
    Json.Encode.list identity [ Json.Encode.string key, oeValue val ]


{-| Encode objects using the `TaggedObject` scheme.
The first argument is a function that, for each possible value `a`, must return a `String` tag
describing it along with an `ObjectEncoding`.
-}
encodeSumTaggedObject : String -> String -> (a -> ( String, ObjectEncoding )) -> a -> Value
encodeSumTaggedObject fieldname contentname mkkeyval v =
    let
        ( key, eval ) =
            mkkeyval v

        kp =
            ( fieldname, Json.Encode.string key )
    in
    case eval of
        EValue val ->
            Json.Encode.object [ kp, ( contentname, val ) ]

        EObject obj ->
            Json.Encode.object (kp :: obj)


{-| Encode objects using the `Untagged` scheme.
-}
encodeSumUntagged : (a -> ( String, ObjectEncoding )) -> a -> Value
encodeSumUntagged mkkeyval v =
    case Tuple.second (mkkeyval v) of
        EValue val ->
            val

        EObject o ->
            Json.Encode.object o


{-| Helper for decoding enum-like sum types
-}
decodeSumNullaries : String -> Dict String a -> Json.Decode.Decoder a
decodeSumNullaries typename mapping =
    Json.Decode.string
        |> Json.Decode.andThen
            (\s ->
                case Dict.get s mapping of
                    Nothing ->
                        Json.Decode.fail ("Could not decode " ++ typename)

                    Just x ->
                        Json.Decode.succeed x
            )


{-| This function is deprecated, use `decodeSumNullaries` (it is the same, only with an appropriate name)
-}
decodeSumUnaries : String -> Dict String a -> Json.Decode.Decoder a
decodeSumUnaries =
    decodeSumNullaries


{-| A convenience function to decode objects that are represented as a sum type containing only nullary or unary constructors
-}



-- Code copied from radix, see https://github.com/bartavelle/json-helpers/issues/4


decodeSumNullaryOrSingleField : String -> Dict String a -> Dict String (Json.Decode.Decoder a) -> Json.Decode.Decoder a
decodeSumNullaryOrSingleField name nullary singlefield =
    Json.Decode.oneOf
        [ decodeSumUnaries name nullary
        , decodeSumObjectWithSingleField name singlefield
        ]


{-| Helper function for decoding map-like objects. It takes a decoder for the key type and a decoder for the value type.
-}
decodeMap : Json.Decode.Decoder comparable -> Json.Decode.Decoder v -> Json.Decode.Decoder (Dict comparable v)
decodeMap decKey decVal =
    let
        decodeKeys =
            resmapM decodeKey

        decodeKey ( k, v ) =
            Result.map (\nk -> ( nk, v )) (Json.Decode.decodeString decKey k)
    in
    Json.Decode.map Dict.fromList (customDecoder (Json.Decode.keyValuePairs decVal) decodeKeys)


{-| Helper function for encoding map-like objects. It takes an encoder for the key type and an encoder for the value type
-}
encodeMap : (comparable -> Value) -> (v -> Json.Encode.Value) -> Dict comparable v -> Json.Encode.Value
encodeMap encKey encVal =
    Json.Encode.dict (Json.Encode.encode 0 << encKey) encVal


{-| An alias to `encodeMap` that is compatible with the naming convention from `elm-bridge`
-}
jsonEncDict : (comparable -> Value) -> (v -> Json.Encode.Value) -> Dict comparable v -> Json.Encode.Value
jsonEncDict =
    encodeMap


{-| An alias to `decodeMap` that is compatible with the naming convention from `elm-bridge`
-}
jsonDecDict : Json.Decode.Decoder comparable -> Json.Decode.Decoder v -> Json.Decode.Decoder (Dict comparable v)
jsonDecDict =
    decodeMap


{-| A helper for set encoding
-}
encodeSet : (comparable -> Json.Encode.Value) -> Set comparable -> Json.Encode.Value
encodeSet =
    Json.Encode.set


{-| A helper for set decoding
-}
decodeSet : Json.Decode.Decoder comparable -> Json.Decode.Decoder (Set comparable)
decodeSet d =
    Json.Decode.map Set.fromList (Json.Decode.list d)


{-| Stolen from NoRedInk's module. Decode a required field.
-}
required : String -> Json.Decode.Decoder a -> Json.Decode.Decoder (a -> b) -> Json.Decode.Decoder b
required key valDecoder decoder =
    custom (Json.Decode.field key valDecoder) decoder


{-| Decodes a field that can be absent from a record. It can also handle fields with a null value.
-}
fnullable : String -> Json.Decode.Decoder a -> Json.Decode.Decoder (Maybe a -> b) -> Json.Decode.Decoder b
fnullable key valDecoder decoder =
    let
        missingfield =
            Json.Decode.maybe (Json.Decode.field key valDecoder)

        nullfield =
            Json.Decode.field key (Json.Decode.nullable valDecoder)
    in
    custom (Json.Decode.oneOf [ missingfield, nullfield ]) decoder


{-| Stolen from NoRedInk's module. Run the given decoder and feed its result into the pipeline at this point.
-}
custom : Json.Decode.Decoder a -> Json.Decode.Decoder (a -> b) -> Json.Decode.Decoder b
custom =
    Json.Decode.map2 (|>)


{-| The (,) operator
-}
tuple2 : a -> b -> ( a, b )
tuple2 a b =
    ( a, b )


{-| The (,,) operator
-}
tuple3 : a -> b -> c -> ( a, b, c )
tuple3 a b c =
    ( a, b, c )

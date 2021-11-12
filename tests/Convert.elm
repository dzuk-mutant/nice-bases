module Convert exposing (..)

import Base exposing (Base, b16, b32, b36)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Bases"
        [ testConversions "b16 conversions" b16 b16List
        , testConversions "b32 conversions" b32 b32List
        , todo "b32rfc conversions"
        , testConversions "b36 conversions" b36 b36List
        , todo "b58 conversions"
        , todo "b62 conversions"
        , todo "b64 conversions"
        , todo "b64url conversions"
        ]


{-| List of Int/String pairs to test b16 conversions.
-}
b16List : List ( Int, String )
b16List =
    [ ( 44000, "abe0" )
    , ( 1337, "539" )
    , ( 923405, "e170d" )
    , ( 76623495, "4912e87" )
    , ( 87234, "154c2" )
    , ( 0, "0" )
    ]


{-| List of Int/String pairs to test b32 conversions.
-}
b32List : List ( Int, String )
b32List =
    [ ( 44000, "1av0" )
    , ( 1337, "19p" )
    , ( 923405, "s5od" )
    , ( 76623495, "292bk7" )
    , ( 87234, "2l62" )
    , ( 0, "0" )
    ]


{-| List of Int/String pairs to test b36 conversions.
-}
b36List : List ( Int, String )
b36List =
    [ ( 44000, "xy8" )
    , ( 1337, "115" )
    , ( 923405, "jsi5" )
    , ( 76623495, "19mb2f" )
    , ( 87234, "1vb6" )
    , ( 0, "0" )
    ]


{-| Tests fromInt and toInt conversions for a list
of int/base string value pairs.
-}
testConversions : String -> Base -> List ( Int, String ) -> Test
testConversions label base tests =
    let
        fromIntExpec =
            \i s -> Expect.equal s (Base.fromInt base i)

        toIntExpec =
            \i s -> Expect.equal (Ok i) (Base.toInt base s)
    in
    describe label
        [ describe "fromInt" <|
            List.indexedMap (singleConversionTest fromIntExpec) tests
        , describe "toInt" <|
            List.indexedMap (singleConversionTest toIntExpec) tests
        ]


{-| Tests a fromInt/toInt conversion from a int/base string pair.
-}
singleConversionTest : (Int -> String -> Expectation) -> Int -> ( Int, String ) -> Test
singleConversionTest expectation index inputs =
    let
        int =
            Tuple.first inputs

        baseStr =
            Tuple.second inputs
    in
    test
        ("Test #" ++ String.fromInt (index + 1))
        (\_ -> expectation int baseStr)

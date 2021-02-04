module Base exposing (b16, b32, b58, b62, b64, fromInt, toInt, StringInputErr(..), convert)

{-| Convert to and from positive numbers of different bases.

# Bases
@docs b16, b32, b58, b62, b64

# Conversions
@docs fromInt, toInt, convert

# Errors

@docs StringInputErr
-}


import Array exposing (Array)


{-| **base16.**

Classic hexadecimal.

    > Base.fromInt b16 44000
    "abe0" : String

This is case insensitive, meaning that if you use `Base.toInt`
it'll interpret upper and lower case as interchangeable.

    > Base.toInt b16 "fe0f"
    Ok 65039

    > Base.toInt b16 "FE0F"
    Ok 65039
-}
b16 : Base
b16 = Base 
    { chars = "0123456789abcdef"
    , cases = CaseAgnostic
    }

{-| **base32.**

This is all numbers and letters in any case, making it both relatively
dense and easy to manually type in.

    > Base.fromInt b32 44000
    "xy8" : String

This is case insensitive, meaning that if you use `Base.toInt`
it'll interpret upper and lower case as the same.

    > Base.toInt b32 "jeff"
    Ok 905163

    > Base.toInt b32 "JEFF"
    Ok 905163
-}
b32 : Base
b32 = Base
    { chars = "0123456789abcdefghijklmnopqrstuvwxyz"
    , cases = CaseAgnostic
    }

{-| **base58.**

This is all numbers and letters excluding visually confusing
combinations - there is no `0`, `O`, `I` or `l`, leaving only
`o` and `1`.

    > Base.fromInt b58 44000
    "E5d"

-}
b58 : Base
b58 = Base
    { chars = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
    , cases = CaseSensitive
    }

{-| **base62.**

All numbers and all cases of letters.

    > Base.fromInt b62 44000
    "BRg"
-}
b62 : Base
b62 = Base 
    { chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    , cases = CaseSensitive
    }

{-| **base64 in the RFC 4648 URL/filename-safe standard.**

All numbers and all cases of letters, plus 62nd and 63rd characters,
which are are `-` and `_` respectively.

    > Base.fromInt b64 44000
    "AlW"
-}
b64 : Base
b64 = Base
    { chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-_"
    , cases = CaseSensitive
    }


{-| Converts a positive Int into non-decimal number string.

    > Base.fromInt b32 69420
    "1hkc"

This does not support negative numbers - 
any negative numbers will be converted
into positive numbers.

    > Base.fromInt b32 -69420
    "1hkc"
-}
fromInt : Base -> Int -> String
fromInt base num = 
    internalFromInt base num

{-| Converts a positive non-decimal number string into an Int.

    > Base.toInt b64 "HAHA"
    Ok 4498506

If your input string is invalid, it will return a
`StringInputErr`. (See the docs for that type for more info.)

This does not support negative values and will likely
throw an error if you introduce them (apart from b64,
because '-' is one of it's characters.).
-}
toInt : Base -> String -> Result StringInputErr Int
toInt base numStr =
    internalToInt base numStr


{-| Converts the string of one non-decimal base number to another.

    > Base.convert b16 b58 "8f0ce9"
    Ok "q3r8"

If your input string is invalid, it will return a `StringInputErr`.
(See the docs for that type for more info.)
-}
convert : Base -> Base -> String -> Result StringInputErr String
convert baseIn baseOut numStr =
    internalToInt baseIn numStr
    |> Result.map (internalFromInt baseOut)



{-| Errors that may occur from converting from Strings.

    > Base.toInt b16 ""
    Err EmptyString

    > Base.toInt b16 "owo"
    Err (BadChars [(0,'o'),(1,'w'),(2,'o')])

- `EmptyString` - The given string is empty.
- `BadChars` - There are characters in the given
string that don't match the spec of the base you used.
It provides a list of each char that failed along with their index in the input string.

-}
type StringInputErr
    = EmptyString
    | BadChars (List (Int, Char))










--------------------------------------------------------------------
--------------------------------------------------------------------
--------------------------------------------------------------------
--------------------------------------------------------------------
--------------------------------------------------------------------
---------------- WELCOME TO BEHIND THE CURTAIN ---------------------
--------------------------------------------------------------------
--------------------------------------------------------------------
--------------------------------------------------------------------
--------------------------------------------------------------------
--------------------------------------------------------------------








{-| The different bases this module supports, it's an opaque type.
-}
type Base =
    Base BaseStuff

type alias BaseStuff =
    { chars : String
    , cases : CaseSensitivity
    }

type CaseSensitivity
    = CaseSensitive
    | CaseAgnostic


{-| Internal. Unwraps the Base opaque type.
-}
unwrap : Base -> BaseStuff
unwrap (Base b) = b



{-| Converts a string of digits for higher than base
10 numbers into an array, ready for use in conversion.
-}
charsToArray : String -> Array Char
charsToArray baseXString =
    baseXString
        |> String.toList
        |> Array.fromList


{-| Maps a base10 Int to a baseX digit
-}
numToDigit : Int -> Array Char -> Char
numToDigit num digits =
    Array.get num digits

    {- this should never happen because
    the previous functons ensure that
    the num is never higher than the
    amount of digits in the array.
    -}
    |> Maybe.withDefault 'x'





{-| Converts an Int into a baseX string.

This does not support negative numbers.
any negative numbers will be converted
into positive numbers.
-}
internalFromInt : Base -> Int -> String
internalFromInt base num = 
    let
        baseXStr = .chars <| unwrap base
    in
        loopingIntConversion (charsToArray baseXStr) [] (abs num)
        |> String.fromList



{-| Internal function that loops over the Int
to create a list of chars in the new base.

NEVER USE A NEGATIVE INT, IT WILL MAKE IT LOOP
INDEFINITELY.
-}
loopingIntConversion : Array Char -> List Char -> Int -> List Char
loopingIntConversion baseXArray results remainder =
    let
        base = Array.length baseXArray
    in
        if remainder < base then
            -- end looping and add the remainder
            (numToDigit remainder baseXArray) :: results
        else
            -- looooooop
            loopingIntConversion
                baseXArray
                (numToDigit (modBy base remainder) baseXArray :: results)
                (remainder // base)











{-| Internal. Maps an indexed baseX digit to an indexed base10 Int.

It returns an indexed Char if it fails for debugging,
or an indexed Int if ir succeeds.
-}
digitToNum : String -> (Int, Char) -> Result (Int, Char) (Int, Int)
digitToNum baseXStr indexedDigitChar =
    let
        index = Tuple.first indexedDigitChar
        char = Tuple.second indexedDigitChar

        attempt = 
            baseXStr
            |> String.toList
            |> Array.fromList
            |> Array.toIndexedList
            |> List.filter (\x -> Tuple.second x == char )
            |> List.head
    in
        case attempt of
            Nothing -> Err (index, char) -- return the bad char
            Just i -> Ok (index, Tuple.first i) -- return the good int


{-| Converts an Int into a baseX string.

This does not support negative numbers.
-}
internalToInt : Base -> String -> Result StringInputErr Int
internalToInt base numStrInitial = 
    let
        digitStr = .chars <| unwrap base
        caseSensitivity = .cases <| unwrap base

        numStr = 
            case caseSensitivity of
                CaseAgnostic -> String.toLower numStrInitial
                CaseSensitive -> numStrInitial
    in
        if String.isEmpty numStr then
            Err EmptyString
        else
            let
                baseX = String.length digitStr
            in
                let 
                    prelimRun =
                        numStr
                        |> String.toList
                        |> Array.fromList
                        |> Array.toIndexedList
                        |> List.map (digitToNum digitStr)
                    
                    failedChars = 
                        prelimRun
                        |> List.filter (\x -> Result.toMaybe x == Nothing)
                
                in
                    if List.isEmpty failedChars then
                         prelimRun
                        -- we can unwrap it now we know it's valid.
                        |> List.map (Result.withDefault (0, 0))

                        -- de-index, reverse the order and index again.
                        -- (we need the powers to be in reverse order to the place of each digit)
                        |> List.map Tuple.second
                        |> List.reverse
                        |> Array.fromList
                        |> Array.toIndexedList

                        -- perform calcs and add
                        |> List.map (\x -> (Tuple.second x * baseX ^ Tuple.first x))
                        |> List.foldl (+) 0
                        |> Ok
                    else
                        failedChars
                        |> List.filterMap
                            (\ x ->
                                case x of
                                    Err e -> Just e
                                    Ok _ -> Nothing
                            )
                        |> BadChars
                        |> Err
                           
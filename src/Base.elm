module Base exposing (b16, b32, b32rfc, b36, b58, b62, b64, b64url, make, CaseSensitivity(..), fromInt, toInt, convert, StringInputErr(..))

{-| Convert to, from and between positive numbers of different bases.

# Bases
@docs b16, b32, b32rfc, b36, b58, b62, b64, b64url

# Conversions
@docs fromInt, toInt, convert

# Errors
@docs StringInputErr

# Create your own
@docs make, CaseSensitivity

-}


import Array exposing (Array)


{-| **base16.**

Classic hexadecimal.

    > Base.fromInt b16 44000
    "abe0" : String

This is case insensitive, meaning that if you use `Base.toInt`
it'll interpret upper and lower case input as the same.

    > Base.toInt b16 "fe0f"
    Ok 65039

    > Base.toInt b16 "FE0F"
    Ok 65039
-}
b16 : Base
b16 = make 
    "0123456789abcdef"
    CaseAgnostic


{-| **base32.**

This is a slightly reduced subset of all numbers and letters,
leaving out `w`, `x`, `y`, and `z`.

    > Base.fromInt b32 44000
    "1av0"

This is case insensitive, meaning that if you use `Base.toInt`
it'll interpret upper and lower case input as the same.
-}
b32 : Base
b32 = make
    "0123456789abcdefghijklmnopqrstuv"
    CaseAgnostic


{-| **base32 in the RFC 4648 standard.**

This is a slightly reduced subset of all numbers and letters, leaving out
visually confusing combinations. The characters are ordered
differently from normal b32.

    > Base.fromInt b32rfc 44000
    "bk7a" 
This is case insensitive, meaning that if you use `Base.toInt`
it'll interpret upper and lower case input as the same.
-}
b32rfc : Base
b32rfc = make
    "abcdefghijklmnopqrstuvwxyz234567"
    CaseAgnostic


{-| **base36.**

This is all numbers and letters in any case, making it both relatively
dense and easy to manually type in.

    > Base.fromInt b36 44000
    "xy8"

This is case insensitive, meaning that if you use `Base.toInt`
it'll interpret upper and lower case input as the same.

    > Base.toInt b36 "jeff"
    Ok 905163

    > Base.toInt b36 "JEFF"
    Ok 905163
-}
b36 : Base
b36 = make
    "0123456789abcdefghijklmnopqrstuvwxyz"
    CaseAgnostic


{-| **base58.**

This is all numbers and letters excluding visually confusing
combinations - there is no `0`, `O`, `I` or `l`, leaving only
`o` and `1`.

    > Base.fromInt b58 44000
    "E5d"

-}
b58 : Base
b58 = make
    "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
    CaseSensitive


{-| **base62.**

All numbers and all cases of letters.

    > Base.fromInt b62 44000
    "BRg"
-}
b62 : Base
b62 = make 
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    CaseSensitive


{-| **base64 in the most common form for text.**

All numbers and all cases of letters, plus 62nd and 63rd characters,
which are are `+` and `/` respectively.

    > Base.fromInt b64 231166
    "4b+"
-}
b64 : Base
b64 = make
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    CaseSensitive


{-| **base64 in the RFC 4648 URL/filename-safe standard.**

All numbers and all cases of letters, plus 62nd and 63rd characters,
which are are `-` and `_` respectively.

    > Base.fromInt b64 231166
    "4b-"
-}
b64url : Base
b64url = make
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
    CaseSensitive




{-| Converts a positive Int into non-decimal number string.

    > Base.fromInt b36 69420
    "1hkc"

This does not support negative numbers - 
any negative numbers will be converted
into positive numbers.

    > Base.fromInt b36 -69420
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






{-| Create a base for non-decimal conversion.

The string input is both the characters you're
mapping digits to (starting from digit 0) and the
base of your resulting numbers (which will be the length of the string).

    b16 : Base
    b16 = make 
        "0123456789abcdef"
        CaseAgnostic

-}
make : String -> CaseSensitivity -> Base
make chars cases =
    Base { chars = chars, cases = cases }




{-| The different bases this module supports, it's an opaque type.
-}
type Base =
    Base BaseStuff

type alias BaseStuff =
    { chars : String
    , cases : CaseSensitivity
    }

{-| Whether or not a base is case-sensitive. This affects whether cases
are relevant when the `toInt` function is used.

- CaseSensitive - Upper-case and lower-case letters are not interchangeable.
- CaseAgnostic - Upper-case and lower-case letters are interchangeable.
-}
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











{-| Converts an Int into a baseX string.

This does not support negative numbers.
-}
internalToInt : Base -> String -> Result StringInputErr Int
internalToInt base numStrInitial = 
    let
        digitStr = .chars <| unwrap base
        caseSensitivity = .cases <| unwrap base
        baseX = String.length digitStr

        -- make it all lowercase if case insensitive
        numStr = case caseSensitivity of
            CaseAgnostic -> String.toLower numStrInitial
            CaseSensitive -> numStrInitial
    in
        if String.isEmpty numStr then
            Err EmptyString
        else
            let 
                prelimRun =
                    numStr
                    |> String.toList
                    |> List.indexedMap (digitToNum digitStr)
                
                failedChars = 
                    prelimRun
                    |> List.filter (\x -> Result.toMaybe x == Nothing)
            
            in
                if List.isEmpty failedChars then
                        prelimRun
                    -- we can unwrap it now we know it's valid.
                    |> List.map (Result.withDefault (0, 0))
                    -- perform calcs to make the conversion work.
                    |> reverseIndex
                    |> List.map (\x -> (Tuple.second x * baseX ^ Tuple.first x))
                    |> List.foldl (+) 0
                    |> Ok
                else
                    failedChars
                    |> List.filterMap
                        (\ x -> case x of
                                Err e -> Just e
                                Ok _ -> Nothing
                        )
                    |> BadChars
                    |> Err
                        

{-| Internal. Maps an indexed baseX digit to an indexed base10 Int.

It returns an indexed Char if it fails for debugging,
or an indexed Int if ir succeeds.
-}
digitToNum : String -> Int -> Char -> Result (Int, Char) (Int, Int)
digitToNum baseXStr index char =
    let
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


{-| Takes an indexed list and reverses the orders of the indexes.
We need this because later calculations require reversed indexes.
-}
reverseIndex : List (Int, Int) -> List (Int, Int)
reverseIndex list = 
    List.map (\x -> (List.length list - Tuple.first x - 1, Tuple.second x ) ) list


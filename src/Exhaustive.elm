module Exhaustive exposing
    ( Generator
    , andThen
    , append
    , array
    , bool
    , char
    , constant
    , customType
    , dict
    , empty
    , field
    , float
    , int
    , list
    , map
    , maybe
    , new
    , pair
    , record
    , result
    , string
    , test
    , triple
    , unit
    , values
    , variant0
    , variant1
    , variant2
    , variant3
    , variant4
    , variant5
    )

import Array
import Dict
import Expect
import Test
import Test.Runner


{-| Turn a `Generator` into a `Test`, for use with `elm-test`.

The API is similar to `elm-test`'s `Test.fuzz`, except that instead of supplying
a fuzzer, you supply a `Generator` from this package.

When the test is executed, `elm-test` will check the expectation against every
term in the sequence, starting from the 0th term and continuing until either an
expectation fails, or the generator is exhausted.

    import Exhaustive exposing (test, int)
    import Test
    import Expect


    test int "This integer is never zero" <|
        \n -> n |> Expect.notEqual 0

    --: Test.Test

-}
test : Generator a -> String -> (a -> Expect.Expectation) -> Test.Test
test gen description toExpectation =
    let
        helper n =
            case gen.nth n of
                Nothing ->
                    Expect.pass

                Just term ->
                    let
                        expectation =
                            toExpectation term
                    in
                    case Test.Runner.getFailureReason expectation of
                        Nothing ->
                            helper (n + 1)

                        Just _ ->
                            expectation
    in
    Test.test
        description
        (\() -> helper 0)


{-| Generate all values of the unit type, `()`. There's only one!
-}
unit : Generator ()
unit =
    constant ()


{-| Generate all boolean values. The 0th term is `False`, the 1st term is `True`.
-}
bool : Generator Bool
bool =
    values [ False, True ]

{-| Generate integer values. 

Since the set of all integers is infinite, we don't 
attempt to generate all of them. Instead, we try a small set of potentially 
interesting integers, which may be useful in tests: 

    import Exhaustive exposing (int)

    int.all ()

    --> [ 0, -1, 1, 2, 3, -10, 10, -100, 100 ]

(I am open to suggestions for more interesting integers than these!)

If you want to generate a specific list of integers, see the docs for `values` 
instead.
-}
int : Generator Int
int =
    values [ 0, -1, 1, 2, 3, -10, 10, -100, 100 ]

{-| Generate `Float` values.

Since the set of all floats is infinite, we don't 
attempt to generate all of them. Instead, we try a small set of potentially 
interesting floats, which may be useful in tests: 

    import Exhaustive exposing (float)

    float.all ()

    --> [ 0.0, -1.0, 1.0, 0.2, 0.3, 0/0, 1/0 ]

(I am open to suggestions for more interesting floats than these!)

If you want to generate a specific list of floats, see the docs for `values` 
instead.
-}
float : Generator Float
float =
    values [ 0.0, -1.0, 1.0, 0.2, 0.3, 0/0, 1/0 ]

{-| Generate `String` values.

Since the set of all `String`s is infinite, we don't 
attempt to generate all of them. Instead, we try a small set of potentially 
interesting `String`s, which may be useful in tests: 

    import Exhaustive exposing (string)

    string.all ()

    --> [ "", " ", "\n", "\u{000D}", "\t", "\"", "a", "ab", "abc" ]

(I am open to suggestions for more interesting `String`s than these!)

If you want to generate a specific list of `String`s, see the docs for `values` 
instead.
-}
string : Generator String
string =
    values [ "", " ", "\n", "\u{000D}", "\t", "\"", "a", "ab", "abc" ]

{-| Generate `Char` values.

Since the set of all `Char`s is infinite, we don't 
attempt to generate all of them. Instead, we try a small set of potentially 
interesting `Char`s, which may be useful in tests: 

    import Exhaustive exposing (char)

    char.all ()

    --> [ ' ', 'a', 'A', '0', '\n', '\t', '\u{000D}' ]

(I am open to suggestions for more interesting `Char`s than these!)

If you want to generate a specific list of `Char`s, see the docs for `values` 
instead.
-}
char : Generator Char
char =
    values [ ' ', 'a', 'A', '0', '\n', '\t', '\u{000D}' ]

{-| Generate all `Maybe` values of a given type.

    import Exhaustive exposing (maybe, bool)

    maybeBool = 
        maybe bool

    maybeBool.all ()

    --> [ Nothing, Just False, Just True ]
-}
maybe : Generator a -> Generator (Maybe a)
maybe a =
    customType
        |> variant0 Nothing
        |> variant1 Just a

{-| Generate all `Result` values of given error and ok types.

    import Exhaustive exposing (result, unit, bool)

    resultUnitBool = 
        result unit bool

    resultUnitBool.all ()

    --> [ Err (), Ok False, Ok True ]
-}
result : Generator error -> Generator value -> Generator (Result error value)
result x a =
    customType
        |> variant1 Err x
        |> variant1 Ok a

{-| Generate all pairs of values of given types.

    import Exhaustive exposing (pair, bool)

    pairBool = 
        pair bool bool

    pairBool.all ()

    --> [ ( False, False ), ( True, False ), ( False, True ), ( True, True ) ]
-}
pair :
    Generator value1
    -> Generator value2
    -> Generator ( value1, value2 )
pair fst snd =
    record Tuple.pair
        |> field fst
        |> field snd

{-| Generate all triples of values of given types.

    import Exhaustive exposing (triple, bool, unit)

    tripleBoolUnitUnit = 
        triple bool unit unit

    tripleBoolUnitUnit.all ()

    --> [ ( False, (), () ), ( True, (), () ) ]
-}
triple :
    Generator value1
    -> Generator value2
    -> Generator value3
    -> Generator ( value1, value2, value3 )
triple fst snd thd =
    record (\a b c -> ( a, b, c ))
        |> field fst
        |> field snd
        |> field thd

{-| Generate all lists of values of a given type, starting with the empty list 
`[]`, and continuing up to lists of a specified length.

    import Exhaustive exposing (list, bool)

    listBool = 
        list 1 bool

    listBool.all ()

    --> [ [], [ False ], [ True ] ]
-}
list : Int -> Generator a -> Generator (List a)
list maxLength item =
    let
        countHelper acc n =
            if n < 0 then
                acc

            else
                countHelper ((item.count ^ n) + acc) (n - 1)

        count =
            countHelper 0 maxLength
    in
    define
        { count = count
        , nth =
            \m ->
                let
                    n =
                        m + 1
                in
                if n > count then
                    Nothing

                else
                    let
                        rotationsHelp periods length =
                            if n <= List.sum ((item.count ^ length) :: periods) then
                                List.foldl
                                    (\period ( n_, output ) ->
                                        ( modBy period n_
                                        , n_ // period :: output
                                        )
                                    )
                                    ( (n - 1) - List.sum periods, [] )
                                    periods
                                    |> Tuple.second

                            else
                                rotationsHelp (item.count ^ length :: periods) (length + 1)

                        rotations =
                            rotationsHelp [] 0
                    in
                    List.foldl
                        (\rs maybeList ->
                            Maybe.map2
                                (\list_ nth -> nth :: list_)
                                maybeList
                                (item.nth rs)
                        )
                        (Just [])
                        rotations
        }

{-| Generate all `Array`s of a given type, starting with an empty `Array` and 
continuing up to `Array`s of a specified length.

    import Exhaustive exposing (array, bool)
    import Array

    arrayBool = 
        array 1 bool

    arrayBool.all ()

    --> [ Array.fromList [], Array.fromList [ False ], Array.fromList [ True ] ]

-}
array : Int -> Generator a -> Generator (Array.Array a)
array maxLength a =
    list maxLength a
        |> map Array.fromList

{-| Generate all `Dict`s of a given key and value type, starting with an empty 
`Dict` and continuing up to `Dict`s of a specified length.

    import Exhaustive exposing (dict, values, unit)
    import Dict

    myDict = 
        dict 1 
            (values [ "a", "b" ])
            unit

    myDict.all ()

    --> [ Dict.fromList [], Dict.fromList [ ( "a", () ) ], Dict.fromList [ ( "b", () ) ] ]
-}
dict :
    Int
    -> Generator comparable
    -> Generator value
    -> Generator (Dict.Dict comparable value)
dict maxLength k v =
    list maxLength (pair k v)
        |> map Dict.fromList


record : constructor -> Generator constructor
record constructor =
    define
        { count = 1
        , nth = \_ -> Just constructor
        }


field :
    Generator value1
    -> Generator (value1 -> value2)
    -> Generator value2
field gen builder =
    define
        { count = gen.count * builder.count
        , nth =
            \n ->
                let
                    rotations =
                        n // builder.count

                    remainder =
                        modBy builder.count n
                in
                case ( builder.nth remainder, gen.nth rotations ) of
                    ( Just left, Just right ) ->
                        Just (left right)

                    _ ->
                        Nothing
        }


customType : Generator a
customType =
    empty


variant0 :
    variant
    -> Generator variant
    -> Generator variant
variant0 variant builder =
    append builder (constant variant)


variant1 :
    (arg -> variant)
    -> Generator arg
    -> Generator variant
    -> Generator variant
variant1 tag arg builder =
    arg
        |> map tag
        |> append builder


variant2 :
    (arg1 -> arg2 -> variant)
    -> Generator arg1
    -> Generator arg2
    -> Generator variant
    -> Generator variant
variant2 tag arg1 arg2 builder =
    let
        variantGen =
            record tag
                |> field arg1
                |> field arg2
    in
    append builder variantGen


variant3 :
    (arg1 -> arg2 -> arg3 -> variant)
    -> Generator arg1
    -> Generator arg2
    -> Generator arg3
    -> Generator variant
    -> Generator variant
variant3 tag arg1 arg2 arg3 builder =
    let
        variantGen =
            record tag
                |> field arg1
                |> field arg2
                |> field arg3
    in
    append builder variantGen


variant4 :
    (arg1 -> arg2 -> arg3 -> arg4 -> variant)
    -> Generator arg1
    -> Generator arg2
    -> Generator arg3
    -> Generator arg4
    -> Generator variant
    -> Generator variant
variant4 tag arg1 arg2 arg3 arg4 builder =
    let
        variantGen =
            record tag
                |> field arg1
                |> field arg2
                |> field arg3
                |> field arg4
    in
    append builder variantGen


variant5 :
    (arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> variant)
    -> Generator arg1
    -> Generator arg2
    -> Generator arg3
    -> Generator arg4
    -> Generator arg5
    -> Generator variant
    -> Generator variant
variant5 tag arg1 arg2 arg3 arg4 arg5 builder =
    let
        variantGen =
            record tag
                |> field arg1
                |> field arg2
                |> field arg3
                |> field arg4
                |> field arg5
    in
    append builder variantGen


type alias Generator value =
    { count : Int
    , nth : Int -> Maybe value
    , all : () -> List value
    , every : Int -> List value
    , sample : Float -> List value
    }


type alias Definition value =
    { count : Int
    , nth : Int -> Maybe value
    }


new : (Int -> Maybe value) -> Generator value
new nth =
    let
        count n =
            case nth n of
                Just _ ->
                    count (n + 1)

                Nothing ->
                    n
    in
    define
        { count = count 0
        , nth = nth
        }


define : Definition value -> Generator value
define definition =
    let
        all () =
            List.range 0 definition.count
                |> List.filterMap definition.nth

        every i =
            everyHelp [] 0 i
                |> List.reverse

        everyHelp output n i =
            if n > toFloat definition.count then
                output

            else
                case definition.nth (round n) of
                    Nothing ->
                        everyHelp output (n + i) (max 1 i)

                    Just nth ->
                        everyHelp (nth :: output) (n + i) (max 1 i)

        sample pct =
            let
                clampedPct =
                    clamp 0.0 1.0 pct
            in
            if clampedPct == 0.0 || definition.count == 0 then
                -- avoid the risk of division by zero!
                []

            else
                (toFloat definition.count / (toFloat definition.count * clampedPct))
                    |> every
    in
    { count = definition.count
    , nth = definition.nth
    , all = all
    , every = toFloat >> every
    , sample = sample
    }


empty : Generator a
empty =
    define
        { count = 0
        , nth = \_ -> Nothing
        }


constant : a -> Generator a
constant value =
    values [ value ]


values : List a -> Generator a
values list_ =
    let
        array_ =
            Array.fromList list_
    in
    define
        { count = List.length list_
        , nth = \n -> Array.get n array_
        }


map :
    (value1 -> value2)
    -> Generator value1
    -> Generator value2
map f gen =
    new (\n -> Maybe.map f (gen.nth n))


andThen : (a -> Generator b) -> Generator a -> Generator b
andThen valueAToGenB genA =
    new
        (\nB ->
            let
                genB =
                    makeGenB (genA.count - 1) empty

                makeGenB nA genBAccumulator =
                    case genA.nth nA of
                        Just valueA ->
                            makeGenB (nA - 1) (append (valueAToGenB valueA) genBAccumulator)

                        Nothing ->
                            genBAccumulator
            in
            genB.nth nB
        )


append :
    Generator value
    -> Generator value
    -> Generator value
append left right =
    define
        { count = left.count + right.count
        , nth =
            \n ->
                if n < left.count then
                    left.nth n

                else
                    right.nth (n - left.count)
        }

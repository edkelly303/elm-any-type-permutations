module Permutations exposing
    ( Definition
    , Generator
    , always
    , append
    , array
    , bool
    , char
    , customType
    , dict
    , empty
    , field
    , float
    , fromList
    , int
    , list
    , map
    , maybe
    , new
    , once
    , record
    , result
    , string
    , triple
    , tuple
    , unit
    , variant0
    , variant1
    , variant2
    , variant3
    , variant4
    , variant5
    )

import Array
import Dict


bool : Generator Bool
bool =
    fromList [ False, True ]


maybe : Generator a -> Generator (Maybe a)
maybe a =
    customType
        |> variant0 Nothing
        |> variant1 Just a


result : Generator error -> Generator value -> Generator (Result error value)
result x a =
    customType
        |> variant1 Err x
        |> variant1 Ok a


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
    new
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
                        (\rs mList ->
                            case mList of
                                Nothing ->
                                    Nothing

                                Just list_ ->
                                    case item.nth rs of
                                        Just v1 ->
                                            Just (v1 :: list_)

                                        Nothing ->
                                            Nothing
                        )
                        (Just [])
                        rotations
        }


unit : Generator ()
unit =
    once ()


int : Generator Int
int =
    fromList [ 0, -1, 1, 2, 3, -10, 10, -100, 100 ]


float : Generator Float
float =
    fromList [ 0.0, -1.0, 1.0, 0.2, 0.3 ]


string : Generator String
string =
    fromList [ "", " ", "\n", "\u{000D}", "\t", "\"", "a", "ab", "abc" ]


char : Generator Char
char =
    fromList [ ' ', 'a', 'A', '0', '\n', '\t', '\u{000D}' ]


tuple :
    Generator value1
    -> Generator value2
    -> Generator ( value1, value2 )
tuple fst snd =
    record Tuple.pair
        |> field fst
        |> field snd


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


array : Int -> Generator a -> Generator (Array.Array a)
array maxLength a =
    list maxLength a
        |> map Array.fromList


dict :
    Int
    -> Generator comparable
    -> Generator value
    -> Generator (Dict.Dict comparable value)
dict maxLength k v =
    list maxLength (tuple k v)
        |> map Dict.fromList


record : constructor -> Generator constructor
record constructor =
    new
        { count = 1
        , nth = \n -> Just constructor
        }


field :
    Generator value1
    -> Generator (value1 -> value2)
    -> Generator value2
field gen builder =
    new
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
    append builder (once variant)


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


new : Definition value -> Generator value
new definition =
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
            if clampedPct == 0 then
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
    new
        { count = 0
        , nth = \_ -> Nothing
        }


always : a -> Generator a
always value =
    new
        { count = 1
        , nth = \_ -> Just value
        }


once : a -> Generator a
once value =
    fromList [ value ]


fromList : List a -> Generator a
fromList list_ =
    let
        array_ =
            Array.fromList list_
    in
    new
        { count = List.length list_
        , nth = \n -> Array.get n array_
        }


map :
    (value1 -> value2)
    -> Generator value1
    -> Generator value2
map f gen =
    new
        { count = gen.count
        , nth = \n -> Maybe.map f (gen.nth n)
        }


append :
    Generator value
    -> Generator value
    -> Generator value
append left right =
    new
        { count = left.count + right.count
        , nth =
            \n ->
                if n < left.count then
                    left.nth n

                else
                    right.nth (n - left.count)
        }

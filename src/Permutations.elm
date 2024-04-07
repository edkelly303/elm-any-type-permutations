module Permutations exposing
    ( bool
    , customType
    , field
    , float
    , int
    , list
    , record
    , string
    , unit
    , variant0
    , variant1
    , variant2
    , variant3
    , variant4
    , variant5
    )

import Gen


bool : Gen.GenTools (List Bool) Bool
bool =
    Gen.fromList [ False, True ]


unit : Gen.GenTools (List ()) ()
unit =
    Gen.once ()


int : Gen.GenTools (List Int) Int
int =
    Gen.fromList [ 0, -1, 1, 2, 3, -10, 10, -100, 100 ]


float : Gen.GenTools (List Float) Float
float =
    Gen.fromList [ 0.0, -1.0, 1.0, 0.2, 0.3 ]


string : Gen.GenTools (List String) String
string =
    Gen.fromList [ "", " ", "\n", "\u{000D}", "\t", "\"", "a", "ab", "abc" ]


type Outcome a
    = Exhausted
    | Expand
    | Continue a


list :
    Int
    -> Int
    -> Gen.GenTools state value
    -> Gen.GenTools (Maybe (List state)) (List value)
list min max item =
    let
        rawInit =
            let
                (Gen.Generator state) =
                    item.init
            in
            state

        rawValue : state -> Gen.Value value
        rawValue state =
            item.value (Gen.Generator state)

        rawNext : state -> state
        rawNext state =
            let
                (Gen.Generator nextState) =
                    item.next (Gen.Generator state)
            in
            nextState

        isNearlyEmpty state =
            (state |> rawNext |> rawValue) == Gen.Empty

        next maybeList =
            case maybeList of
                Nothing ->
                    Nothing

                Just list_ ->
                    case list_ of
                        [] ->
                            {- an empty list is only possible if `item` is a
                               generator initialised with a `min` of 0 - it will
                               be the initial state of such a generator.
                            -}
                            if max > 0 then
                                Just [ rawInit ]

                            else
                                Nothing

                        gen :: gens ->
                            case recurse (List.length list_) gen gens of
                                Exhausted ->
                                    Nothing

                                Expand ->
                                    Just (List.repeat (List.length list_ + 1) rawInit)

                                Continue a ->
                                    Just a

        recurse currentLength gen gens =
            case gens of
                [] ->
                    if gen |> isNearlyEmpty then
                        if max > currentLength then
                            Expand

                        else
                            Exhausted

                    else
                        Continue [ rawNext gen ]

                nextGen :: restGens ->
                    if gen |> isNearlyEmpty then
                        case recurse currentLength nextGen restGens of
                            Exhausted ->
                                Exhausted

                            Continue newGens ->
                                Continue (rawInit :: newGens)

                            Expand ->
                                Expand

                    else
                        Continue (rawNext gen :: gens)
    in
    Gen.new
        { count = (item.count ^ (max + 1)) - (item.count ^ min)
        , init =
            let
                (Gen.Generator init) =
                    item.init
            in
            Just (List.repeat min init)
        , next = next
        , value =
            \maybeList ->
                case maybeList of
                    Nothing ->
                        Gen.Empty

                    Just state ->
                        List.foldl
                            (\itemState output ->
                                case item.value (Gen.Generator itemState) of
                                    Gen.Value v ->
                                        v :: output

                                    Gen.Empty ->
                                        output
                            )
                            []
                            state
                            |> Gen.Value
        }


record : a -> Gen.GenTools (List a) a
record constructor =
    Gen.once constructor


field : Gen.GenTools state2 value1 -> Gen.GenTools state1 (value1 -> value2) -> Gen.GenTools ( state1, state2 ) value2
field fieldType builder =
    Gen.andMap fieldType builder


customType : Gen.GenTools () a
customType =
    Gen.empty



-- variant0 :
--     variant
--     -> Gen.GenTools builderState variant
--     -> Gen.GenTools ( builderState, () ) variant


variant0 variant builder =
    Gen.append builder (Gen.once variant)



-- variant1 :
--     (arg -> variant)
--     -> Gen.GenTools argState arg
--     -> Gen.GenTools builderState variant
--     -> Gen.GenTools ( builderState, argState ) variant


variant1 tag arg builder =
    arg
        |> Gen.map tag
        |> Gen.append builder



-- variant2 :
--     (arg1 -> arg2 -> variant)
--     -> Gen.GenTools arg1State arg1
--     -> Gen.GenTools arg2State arg2
--     -> Gen.GenTools builderState variant
--     -> Gen.GenTools ( builderState, ( ( (), arg1State ), arg2State ) ) variant


variant2 tag arg1 arg2 builder =
    let
        variantGen =
            Gen.once tag
                |> Gen.andMap arg1
                |> Gen.andMap arg2
    in
    Gen.append builder variantGen



-- variant3 :
--     (arg1 -> arg2 -> arg3 -> variant)
--     -> Gen.GenTools arg1State arg1
--     -> Gen.GenTools arg2State arg2
--     -> Gen.GenTools arg3State arg3
--     -> Gen.GenTools builderState variant
--     -> Gen.GenTools ( builderState, ( ( ( (), arg1State ), arg2State ), arg3State ) ) variant


variant3 tag arg1 arg2 arg3 builder =
    let
        variantGen =
            Gen.once tag
                |> Gen.andMap arg1
                |> Gen.andMap arg2
                |> Gen.andMap arg3
    in
    Gen.append builder variantGen



-- variant4 :
--     (arg1 -> arg2 -> arg3 -> arg4 -> variant)
--     -> Gen.GenTools arg1State arg1
--     -> Gen.GenTools arg2State arg2
--     -> Gen.GenTools arg3State arg3
--     -> Gen.GenTools arg4State arg4
--     -> Gen.GenTools builderState variant
--     -> Gen.GenTools ( builderState, ( ( ( ( (), arg1State ), arg2State ), arg3State ), arg4State ) ) variant


variant4 tag arg1 arg2 arg3 arg4 builder =
    let
        variantGen =
            Gen.once tag
                |> Gen.andMap arg1
                |> Gen.andMap arg2
                |> Gen.andMap arg3
                |> Gen.andMap arg4
    in
    Gen.append builder variantGen



-- variant5 :
--     (arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> variant)
--     -> Gen.GenTools arg1State arg1
--     -> Gen.GenTools arg2State arg2
--     -> Gen.GenTools arg3State arg3
--     -> Gen.GenTools arg4State arg4
--     -> Gen.GenTools arg5State arg5
--     -> Gen.GenTools builderState variant
--     -> Gen.GenTools ( builderState, ( ( ( ( ( (), arg1State ), arg2State ), arg3State ), arg4State ), arg5State ) ) variant


variant5 tag arg1 arg2 arg3 arg4 arg5 builder =
    let
        variantGen =
            Gen.once tag
                |> Gen.andMap arg1
                |> Gen.andMap arg2
                |> Gen.andMap arg3
                |> Gen.andMap arg4
                |> Gen.andMap arg5
    in
    Gen.append builder variantGen

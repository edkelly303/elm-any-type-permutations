module Gen exposing (Definition, GenTools, Generator(..), Value(..), always, andMap, append, empty, fromList, list, map, merge, new, once)


type Generator state value
    = Generator state


type Value a
    = Value a
    | Empty


type alias GenTools state value =
    { count : Int
    , init : Generator state value
    , next : Generator state value -> Generator state value
    , value : Generator state value -> Value value
    , all : () -> List value
    , take : Int -> Generator state value -> List value
    , drop : Int -> Generator state value -> Generator state value
    }


type alias Definition state value =
    { count : Int
    , init : state
    , next : state -> state
    , value : state -> Value value
    }


new : Definition state value -> GenTools state value
new definition =
    let
        init =
            Generator definition.init

        next (Generator state) =
            Generator (definition.next state)

        value (Generator state) =
            definition.value state

        all () =
            allHelp [] init

        allHelp output state =
            case value state of
                Value v ->
                    allHelp (v :: output) (next state)

                Empty ->
                    List.reverse output

        take n state =
            takeHelp [] n state

        takeHelp output n state =
            if n > 0 then
                case value state of
                    Value v ->
                        takeHelp (v :: output) (n - 1) (next state)

                    Empty ->
                        List.reverse output

            else
                List.reverse output

        drop n state =
            if n > 0 then
                drop (n - 1) (next state)

            else
                state
    in
    { count = definition.count
    , init = init
    , next = next
    , value = value
    , all = all
    , take = take
    , drop = drop
    }


empty : GenTools () a
empty =
    new
        { count = 1
        , init = ()
        , next = identity
        , value = \_ -> Empty
        }


always : a -> GenTools () a
always value =
    new
        { count = 1
        , init = ()
        , next = identity
        , value = \_ -> Value value
        }


once : a -> GenTools (List a) a
once value =
    fromList [ value ]


fromList : List a -> GenTools (List a) a
fromList list_ =
    new
        { count = List.length list_
        , init = list_
        , next =
            \state ->
                case state of
                    _ :: rest ->
                        rest

                    [] ->
                        []
        , value =
            \state ->
                case state of
                    val :: _ ->
                        Value val

                    [] ->
                        Empty
        }


type Outcome a
    = Exhausted
    | Expand
    | Continue a


list :
    Int
    -> Int
    -> GenTools state value
    -> GenTools (Maybe (List state)) (List value)
list min max item =
    let
        rawInit =
            let
                (Generator state) =
                    item.init
            in
            state

        rawValue : state -> Value value
        rawValue state =
            item.value (Generator state)

        rawNext : state -> state
        rawNext state =
            let
                (Generator nextState) =
                    item.next (Generator state)
            in
            nextState

        isNearlyEmpty state =
            (state |> rawNext |> rawValue) == Empty

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

        counter acc n =
            if n < min then
                acc

            else
                counter ((item.count ^ n) + acc) (n - 1)
    in
    new
        { count = counter 0 max
        , init =
            let
                (Generator init) =
                    item.init
            in
            Just (List.repeat min init)
        , next = next
        , value =
            \maybeList ->
                case maybeList of
                    Nothing ->
                        Empty

                    Just state ->
                        List.foldl
                            (\itemState output ->
                                case item.value (Generator itemState) of
                                    Value v ->
                                        v :: output

                                    Empty ->
                                        output
                            )
                            []
                            state
                            |> Value
        }


andMap :
    GenTools state2 value1
    -> GenTools state1 (value1 -> value2)
    -> GenTools ( state1, state2 ) value2
andMap right left =
    merge (\f a -> f a) left right


merge :
    (value1 -> value2 -> value3)
    -> GenTools state1 value1
    -> GenTools state2 value2
    -> GenTools ( state1, state2 ) value3
merge merger left right =
    new
        { count = left.count * right.count
        , init =
            let
                (Generator initLeft) =
                    left.init

                (Generator initRight) =
                    right.init
            in
            ( initLeft, initRight )
        , next =
            \( stateLeft, stateRight ) ->
                let
                    (Generator nextRight) =
                        right.next (Generator stateRight)
                in
                case right.value (Generator nextRight) of
                    Value _ ->
                        ( stateLeft, nextRight )

                    Empty ->
                        let
                            (Generator nextLeft) =
                                left.next (Generator stateLeft)

                            (Generator initRight) =
                                right.init
                        in
                        ( nextLeft, initRight )
        , value =
            \( stateLeft, stateRight ) ->
                case ( left.value (Generator stateLeft), right.value (Generator stateRight) ) of
                    ( Value leftValue, Value rightValue ) ->
                        Value (merger leftValue rightValue)

                    _ ->
                        Empty
        }


map :
    (value1 -> value2)
    -> GenTools state value1
    -> GenTools state value2
map f tools =
    new
        { count = tools.count
        , init =
            let
                (Generator state) =
                    tools.init
            in
            state
        , next =
            \state ->
                let
                    (Generator newState) =
                        tools.next (Generator state)
                in
                newState
        , value =
            \state ->
                case tools.value (Generator state) of
                    Value v ->
                        Value (f v)

                    Empty ->
                        Empty
        }


append :
    GenTools state1 value
    -> GenTools state2 value
    -> GenTools ( state1, state2 ) value
append left right =
    new
        { count = left.count + right.count
        , init =
            let
                (Generator initLeft) =
                    left.init

                (Generator initRight) =
                    right.init
            in
            ( initLeft, initRight )
        , next =
            \( stateLeft, stateRight ) ->
                case left.value (Generator stateLeft) of
                    Value v ->
                        let
                            (Generator nextStateLeft) =
                                left.next (Generator stateLeft)
                        in
                        ( nextStateLeft, stateRight )

                    Empty ->
                        let
                            (Generator nextStateRight) =
                                right.next (Generator stateRight)
                        in
                        ( stateLeft, nextStateRight )
        , value =
            \( stateLeft, stateRight ) ->
                let
                    valueLeft =
                        left.value (Generator stateLeft)

                    valueRight =
                        right.value (Generator stateRight)
                in
                case ( valueLeft, valueRight ) of
                    ( Value v, _ ) ->
                        Value v

                    ( Empty, Value v ) ->
                        Value v

                    ( Empty, Empty ) ->
                        Empty
        }

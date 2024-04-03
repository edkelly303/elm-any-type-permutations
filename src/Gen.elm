module Gen exposing (Definition, GenTools, Generator(..), Value(..), always, andMap, append, empty, fromList, map, merge, new, once)


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
fromList list =
    new
        { count = List.length list
        , init = list
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

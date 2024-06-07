module Example exposing (..)

import Exhaustive as E
import Expect
import Test exposing (..)


list =
    E.list 10 (E.values [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ])


monad =
    E.values [ "a", "b", "c" ]
        |> E.andThen
            (\str ->
                if str == "a" then
                    E.values [ 1, 2, 3, 4 ]

                else if str == "b" then
                    E.values [ 5, 6, 7, 8, 9 ]

                else
                    E.values [ 10, 11 ]
            )


suite : Test
suite =
    describe "Tests!"
        [ E.test list "List isn't [ 1, 2, 3, 4, 5 ]" <| \l -> l |> Expect.notEqual [ 1, 2, 3, 4, 5 ]
        , E.test monad "Monad can't be 11" <| \m -> m |> Expect.notEqual 11
        ]

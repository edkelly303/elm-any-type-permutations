module Example exposing (..)

import Expect
import Exhaustive as E
import Test exposing (..)


list =
    E.list 10 (E.values [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ])


suite : Test
suite =
    E.test list "List isn't [ 1, 2, 3, 4, 5 ]" <| \l -> l |> Expect.notEqual [ 1, 2, 3, 4, 5 ]

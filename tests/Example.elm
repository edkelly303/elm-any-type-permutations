module Example exposing (..)

import Expect
import Permutations as P
import Test exposing (..)


list =
    P.list 10 (P.oneOf [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ])


suite : Test
suite =
    P.test list "List isn't [ 1, 2, 3, 4, 5 ]" <| \l -> l |> Expect.notEqual [ 1, 2, 3, 4, 5 ]

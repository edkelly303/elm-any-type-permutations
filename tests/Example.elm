module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Permutations as P
import Test exposing (..)


suite : Test
suite =
    P.test P.int "Int" <| \int -> Expect.notEqual int 0

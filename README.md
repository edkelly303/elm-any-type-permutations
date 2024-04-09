Generate permutations of values for any Elm type.

For finite types such as `Bool` and `()`, we can exhaustively generate every possible value.

For infinite types such as `Int`, `Float` and `String`, we generate permutations of a small set of "interesting" values.
For example, `int` generates the values 0, -1, 1, 2, 3, -10, 10, -100, 100.

For complex types such as tuples, records, custom types and lists, the exhaustiveness depends on whether the contents
are finite or infinite. For example, we can generate all possible permutations of `tuple bool bool`, but only a sample
of interesting permutations for `tuple string int`.

```elm
import Permutations exposing (tuple, bool, list, record, field, unit, customType, variant0, variant1)

tup =
    tuple bool bool

tup.all ()

--> [ ( False, False ), ( False, True ), ( True, False ), ( True, True ) ]

tup.count 

--> 4

generator = 
    tup.init

tup.value generator

--> Just ( False, False )

generator 
    |> tup.next
    |> tup.value

--> Just ( False, True )

tup.take 2 generator

--> [ ( False, False ), ( False, True ) ]

generator
    |> tup.drop 2
    |> tup.take 2

--> [ ( True, False ), ( True, True ) ]

boolList =
    list { minLength = 0, maxLength = 2 } bool

boolList.all ()

--> [ [], [ False ], [ True ], [ False, False ], [ False, True ], [ True, False ], [ True, True ] ]

type alias Rec =
    { bool : Bool
    , unit : () 
    }

rec =
    record Rec
        |> field bool
        |> field unit

rec.all ()

--> [ { bool : False, unit : () }, { bool : True, unit : () } ]

type Foo
    = Bar
    | Baz Bool

foo =
    customType
        |> variant0 Bar
        |> variant1 Baz bool

foo.all ()

--> [ Bar, Baz False, Baz True ]
```

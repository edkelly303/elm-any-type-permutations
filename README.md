# Elm Exhaustive Generators

Create exhaustive generators for arbitrarily complex Elm types.

For finite types such as `Bool` and `()`, we can exhaustively generate every possible value.

For infinite types such as `Int`, `Float` and `String`, by default we generate only a small set of potentially "interesting" values. 
For example, `int` generates the values 0, -1, 1, 2, 3, -10, 10, -100, 100.

For complex types such as pairs, triples, records and custom types, the exhaustiveness depends on whether the contents
are finite or infinite. For example, we can generate all possible values of `pair bool bool`, but only a sample
of interesting values for `pair string int`.

For collection types such as `List a` and `Dict k v`, we can specify the maximum length of the collection.
For example, `list 3 unit` will generate every possible value of `List ()` from the 
empty list `[]` up to `[ (), (), () ]`.

## Examples

```elm
import Exhaustive exposing (pair, bool, list, record, field, unit, customType, variant0, variant1)

-- Create an exhaustive generator - for example, for a pair of booleans `( Bool, Bool )`

twoBools =
    pair bool bool

-- Count the total number of values that the generator can produce

twoBools.count

--> 4

-- Generate the first value (the 0th term of the sequence)

twoBools.nth 0

--> Just ( False, False )

-- Generate a list of all possible values, starting from the 0th term of the sequence

twoBools.all ()

--> [ ( False, False ), ( True, False ), ( False, True ), ( True, True ) ]

-- Get every second value from the generator, starting with the 0th term of the sequence

twoBools.every 2

--> [ ( False, False ), ( False, True ) ]

-- Take a sample of the values from the generator. Provide a percentage between 0.0 and 1.0
-- to determine what proportion of the full sequence you want to include in your sample.
-- The sample will always start with the 0th term of the sequence

twoBools.sample 0.75

--> [ ( False, False ), ( True, False ), ( True, True ) ]

-- Create a generator for lists of boolean values, from the empty list up to lists of length 2

boolList =
    list 2 bool

boolList.all ()

--> [ [], [ False ], [ True ], [ False, False ], [ False, True ], [ True, False ], [ True, True ] ]

-- Create a generator for a record type

type alias Rec =
    { bool : Bool
    , unit : () 
    }

rec =
    record Rec
        |> field bool
        |> field unit

rec.all ()

--> [ { bool = False, unit = () }, { bool = True, unit = () } ]

-- Create a generator for a custom type

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

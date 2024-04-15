Generate permutations of values for any Elm type.

For finite types such as `Bool` and `()`, we can exhaustively generate every possible value.

For infinite types such as `Int`, `Float` and `String`, we generate permutations of a small set of "interesting" values.
For example, `int` generates the values 0, -1, 1, 2, 3, -10, 10, -100, 100.

For complex types such as tuples, records and custom types, the exhaustiveness depends on whether the contents
are finite or infinite. For example, we can generate all possible permutations of `tuple bool bool`, but only a sample
of interesting permutations for `tuple string int`.

For collection types such as `List a` and `Dict k v`, we can specify the maximum length of the collection.
For example, `list 3 unit` will generate every permutation of `List ()` from the 
empty list `[]` up to `[ (), (), () ]`.

```elm
import Permutations exposing (tuple, bool, list, record, field, unit, customType, variant0, variant1)

-- Create a permutation generator for a pair of booleans:

tup =
    tuple bool bool

-- Generate a list of all permutations:

tup.all ()

--> [ ( False, False ), ( True, False ), ( False, True ), ( True, True ) ]

-- Count the number of permutations:

tup.count

--> 4

-- Get the nth term in the sequence of permutations:
-- (Note: the first term in the sequence is at n = 0, not n = 1)

tup.nth 1

--> Just (True, False)

-- Get every xth permutation from the sequence, starting with the first term:

tup.every 2

--> [ ( False, False ), ( False, True ) ]

-- Take a sample of terms from the sequence by providing a percentage between 0.0 and 1.0:
-- For example, if `x.count` is 100, `x.sample 0.5` will return a list of 50 evenly spaced terms, starting with the 0th term.

tup.sample 0.75

--> [ ( False, False ), ( True, False ), ( True, True ) ]
```

-- Create a generator for all lists of boolean values from the empty list up to lists of length 2:

boolList =
    list 2 bool

boolList.all ()

--> [ [], [ False ], [ True ], [ False, False ], [ False, True ], [ True, False ], [ True, True ] ]


-- -- Create a generator for a record type:

-- type alias Rec =
--     { bool : Bool
--     , unit : () 
--     }

-- rec =
--     record Rec
--         |> field bool
--         |> field unit

-- --rec.all ()

-- --> [ { bool : False, unit : () }, { bool : True, unit : () } ]

-- -- Create a generator for a custom type:

-- type Foo
--     = Bar
--     | Baz Bool

-- foo =
--     customType
--         |> variant0 Bar
--         |> variant1 Baz bool

-- -- foo.all ()

-- --> [ Bar, Baz False, Baz True ]
```

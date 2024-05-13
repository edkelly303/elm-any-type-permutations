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

## Examples

### Create a permutation generator
For example, for a tuple of booleans `( Bool, Bool )`:
```elm
tup =
    tuple bool bool
```
### Generate all permutations
This will give you a list of all permutations, starting from the 0th term in the sequence
```elm
tup.all ()

--> [ ( False, False ), ( True, False ), ( False, True ), ( True, True ) ]
```
### Count the total number of permutations
```elm
tup.count

--> 4
```
### Get the nth term in the sequence of permutations
Note: the first term in the sequence is at n = 0, not n = 1
```elm
tup.nth 0

--> Just ( False, False )
```
### Get every xth permutation from the sequence, starting with the 0th term
```elm
tup.every 2

--> [ ( False, False ), ( False, True ) ]
```
### Take a sample of the sequence
Provide a percentage between 0.0 and 1.0 to determine what proportion of the full sequence you want to include in your sample. 

For example, `x.sample 0.5` will return half of the terms in the sequence. If `x.count` is 100, you will get a list of 50 evenly spaced terms, starting with the 0th term.
```elm
tup.sample 0.75

--> [ ( False, False ), ( True, False ), ( True, True ) ]
```
### Create generators for lists
This will generate lists of boolean values from the empty list up to lists of length 2:
```elm
boolList =
    list 2 bool

boolList.all ()

--> [ [], [ False ], [ True ], [ False, False ], [ False, True ], [ True, False ], [ True, True ] ]
```
### Create generators for records
```elm
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
```
### Create generators for custom types
```elm
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

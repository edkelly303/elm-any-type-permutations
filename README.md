Generate all permutations for any Elm type

```elm
t =
    tuple bool bool

t.all ()

--> [ ( False, False ), ( False, True ), ( True, False ), ( True, True ) ]

u =
    list { minLength = 0, maxLength = 2 } bool

u.all ()

--> [ [], [ False ], [ True ], [ False, False ], [ False, True ], [ True, False ], [ True, True ] ]

type alias R =
  { bool : Bool, unit : () }

r =
    record R
        |> field bool
        |> field unit

r.all ()

--> [ { bool : False, unit : () }, { bool : True, unit : () } ]

type Foo
    = Bar
    | Baz Bool

c =
    customType
        |> variant0 Bar
        |> variant1 Baz bool

c.all ()

--> [ Bar, Baz False, Baz True ]
```

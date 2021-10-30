module OData4.Url exposing
    ( QueryOption, select, filter, orderBy, top, skip
    , Order, asc, desc
    , CommonExpression
    , eq, ne, gt, ge, lt, le, and, or, not_, in_
    , startsWith, endsWith, contains
    , any, all
    , Value, null, true, false, int, float, string, date, dateTime, guid
    , plainStringOperator, customValue
    , toQueryParameter
    , test
    )

{-| Build Open Data Protocol (OData v4) queries in Elm.

This package supports a _subset_ of OData 4.01
[_query option_](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_CommonExpressionSyntax),
[_common expression_](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_CommonExpressionSyntax), and
[_primitive literal_](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_PrimitiveLiterals).

<http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html>

<http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html>

  - If you need a _query option_, _common expression_, or _primitive literal_
    that is missing in this package undeservedly, suggest a PR
  - If what you need is missing in this package and it's ok, or it deviates from the standard,
    or [other](https://github.com/microsoftgraph/microsoft-graph-docs/issues/14547), then for:
      - _query option_, append custom [`Url.Builder.QueryParameter`](https://package.elm-lang.org/packages/elm/url/latest/Url-Builder#QueryParameter) (see example)
      - _common expression_, use [`plainStringOperator`](#plainStringOperator) (see example)
      - _primitive literal_, use [`customValue`](#customValue) (see example)


# Example

    import Time
    import Url
    import Url.Builder

    -- Custom value function
    dateAsString : Time.Posix -> Value
    dateAsString posix =
        customValue (\s -> "'" ++ s ++ "'") (date posix)

    [ filter
        ( or
            [ eq "City" (string "Tallinn")
            , eq "City" (string "Singapore")
            -- Custom operator
            , plainStringOperator "concat(concat(City,', '), Country) eq 'Berlin, Germany'"
            -- Custom value
            , ge "start/dateTime" (dateAsString (Time.millisToPosix 1631124861000))
            ]
        )
    , top 20
    , skip 40
    , orderBy [ ("Created", Just desc), ("City", Just asc) ]
    , select [ "Id", "City", "Created", "Body" ]
    ]
    |> List.map toQueryParameter
    -- Custom query option
    |> List.append [ Url.Builder.string "$search" "blue OR green" ]
    |> Url.Builder.toQuery >> Url.percentDecode
    --> Just (String.join ""
    --> [ "?$search=blue OR green&"
    --> , "$filter=City eq 'Tallinn' or "
    --> ,   "City eq 'Singapore' or "
    --> ,   "concat(concat(City,', '), Country) eq 'Berlin, Germany' or "
    --> ,   "start/dateTime ge '2021-09-08'&"
    --> , "$top=20&"
    --> , "$skip=40&"
    --> , "$orderBy=Created desc,City asc&"
    --> , "$select=Id,City,Created,Body"
    --> ])


# Query Options

@docs QueryOption, select, filter, orderBy, top, skip


# Order

@docs Order, asc, desc


# Common Expression Syntax [[doc](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_CommonExpressionSyntax)]

@docs CommonExpression


## Logical Operators [[doc](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_LogicalOperators)]

@docs eq, ne, gt, ge, lt, le, and, or, not_, in_


## String Functions [[doc](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_StringandCollectionFunctions)]

@docs startsWith, endsWith, contains


## Lambda Operators [[doc](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_LambdaOperators)]

@docs any, all


# Primitive Literals [[doc](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_PrimitiveLiterals)]

@docs Value, null, true, false, int, float, string, date, dateTime, guid


## Custom

@docs plainStringOperator, customValue


# Building Url.Builder.QueryParameter

@docs toQueryParameter


# Test internals

@docs test

-}

import OData4.Internals
import Time
import Url.Builder


{-| Test internals are needed for [`elm-verify-examples`](https://github.com/stoeffel/elm-verify-examples)
-}
test :
    { stringFromValue : Value -> String
    }
test =
    { stringFromValue = stringFromValue
    }



-- Query Options


{-| [System query options](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_SystemQueryOptions)
are query string parameters that control the amount and order of the data returned for the resource identified by the URL.
-}
type QueryOption
    = Select (List String)
    | Filter CommonExpression
    | OrderBy (List ( String, Maybe Order ))
    | Top Int
    | Skip Int


{-| The [`$select`](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html#sec_SystemQueryOptionselect)
system query option requests that the service return only the properties explicitly requested by the client.

    import Url
    import Url.Builder

    select [ "id", "subject", "body" ]
    |> toQueryParameter >> List.singleton >> Url.Builder.toQuery >> Url.percentDecode
    --> Just "?$select=id,subject,body"

-}
select : List String -> QueryOption
select attrs =
    Select attrs


{-| The [`$filter`](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html#sec_SystemQueryOptionfilter)
system query option restricts the set of items returned.

    import Url
    import Url.Builder

    filter (endsWith "mail" "@hotmail.com")
    |> toQueryParameter >> List.singleton >> Url.Builder.toQuery >> Url.percentDecode
    --> Just "?$filter=endswith(mail,'@hotmail.com')"

-}
filter : CommonExpression -> QueryOption
filter operator =
    Filter operator


{-| The [`$orderby`](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html#sec_SystemQueryOptionorderby)
System Query option specifies the order in which items are returned from the service.

    import Url
    import Url.Builder

    orderBy [ ("created", Just desc), ("displayName", Just asc) ]
    |> toQueryParameter >> List.singleton >> Url.Builder.toQuery >> Url.percentDecode
    --> Just "?$orderBy=created desc,displayName asc"

-}
orderBy : List ( String, Maybe Order ) -> QueryOption
orderBy attrs =
    OrderBy attrs


{-| The [`$top`](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html#sec_SystemQueryOptiontop)
query parameter requests the number of items in the queried collection to be included in the result.
A client can request a particular page of items by combining `$top` and `$skip`.

    import Url
    import Url.Builder

    top 10
    |> toQueryParameter >> List.singleton >> Url.Builder.toQuery >> Url.percentDecode
    --> Just "?$top=10"

-}
top : Int -> QueryOption
top i =
    Top i


{-| The [`$skip`](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html#sec_SystemQueryOptionskip)
query parameter requests the number of items in the queried collection that are to be skipped and not included in the result.
A client can request a particular page of items by combining `$top` and `$skip`.

    import Url
    import Url.Builder

    skip 10
    |> toQueryParameter >> List.singleton >> Url.Builder.toQuery >> Url.percentDecode
    --> Just "?$skip=10"

-}
skip : Int -> QueryOption
skip i =
    Skip i



-- Order


{-| The expression _can_ include the suffix
[`asc`](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html#sec_SystemQueryOptionorderby) for ascending or
[`desc`](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part1-protocol.html#sec_SystemQueryOptionorderby) for descending.
Defaults to `asc`.
-}
type Order
    = Asc
    | Desc


stringFromOrder : Order -> String
stringFromOrder order =
    case order of
        Asc ->
            "asc"

        Desc ->
            "desc"


{-| Ascending order

    import Url
    import Url.Builder

    orderBy [ ("displayName", Just asc) ]
    |> toQueryParameter >> List.singleton >> Url.Builder.toQuery >> Url.percentDecode
    --> Just "?$orderBy=displayName asc"

    orderBy [ ("displayName", Nothing) ]
    |> toQueryParameter >> List.singleton >> Url.Builder.toQuery >> Url.percentDecode
    --> Just "?$orderBy=displayName"

-}
asc : Order
asc =
    Asc


{-| Descending order

    import Url
    import Url.Builder

    orderBy [ ("created", Just desc) ]
    |> toQueryParameter >> List.singleton >> Url.Builder.toQuery >> Url.percentDecode
    --> Just "?$orderBy=created desc"

    orderBy [ ("created", Nothing) ]
    |> toQueryParameter >> List.singleton >> Url.Builder.toQuery >> Url.percentDecode
    --> Just "?$orderBy=created"

-}
desc : Order
desc =
    Desc



-- Common Expressions


{-| The following operators, functions, and literals can be used in `$filter` and `$orderby`
-}
type
    CommonExpression
    -- Logical Operators
    = EQ String Value
    | NE String Value
    | GT String Value
    | GE String Value
    | LT String Value
    | LE String Value
    | And (List CommonExpression)
    | Or (List CommonExpression)
    | Not CommonExpression
    | In String (List Value)
      -- String Functions
    | Contains String String
    | EndsWith String String
    | StartsWith String String
      -- Lambda Operators
    | Any String CommonExpression
    | All String CommonExpression
      -- Custom
    | PlainString String


{-| Equals

The `null` value is equal to itself, and only to itself.

    import Url
    import Url.Builder

    filter (eq "name" (string "Luna"))
    |> toQueryParameter >> List.singleton >> Url.Builder.toQuery >> Url.percentDecode
    --> Just "?$filter=name eq 'Luna'"

-}
eq : String -> Value -> CommonExpression
eq key value =
    EQ key value


{-| Not Equals

    import Url
    import Url.Builder

    filter (ne "deleted" null)
    |> toQueryParameter >> List.singleton >> Url.Builder.toQuery >> Url.percentDecode
    --> Just "?$filter=deleted ne null"

-}
ne : String -> Value -> CommonExpression
ne key value =
    NE key value


{-| Greater Than

    import Url
    import Url.Builder

    filter (gt "age" (int 14))
    |> toQueryParameter >> List.singleton >> Url.Builder.toQuery >> Url.percentDecode
    --> Just "?$filter=age gt 14"

-}
gt : String -> Value -> CommonExpression
gt key value =
    GT key value


{-| Greater Than or Equal

    import Url
    import Url.Builder

    filter (ge "age" (int 14))
    |> toQueryParameter >> List.singleton >> Url.Builder.toQuery >> Url.percentDecode
    --> Just "?$filter=age ge 14"

-}
ge : String -> Value -> CommonExpression
ge key value =
    GE key value


{-| Less Than

    import Url
    import Url.Builder

    filter (lt "age" (int 14))
    |> toQueryParameter >> List.singleton >> Url.Builder.toQuery >> Url.percentDecode
    --> Just "?$filter=age lt 14"

-}
lt : String -> Value -> CommonExpression
lt key value =
    LT key value


{-| Less Than or Equal

    import Url
    import Url.Builder

    filter (le "age" (int 14))
    |> toQueryParameter >> List.singleton >> Url.Builder.toQuery >> Url.percentDecode
    --> Just "?$filter=age le 14"

-}
le : String -> Value -> CommonExpression
le key value =
    LE key value


{-| The `and` operator returns `true` if both the left and right operands evaluate to `true`, otherwise it returns `false`.

The `null` value is treated as unknown, so if one operand evaluates to `null` and the other operand to `false`, the and operator returns `false`.
All other combinations with `null` return `null`.

    import Url
    import Url.Builder

    filter (and [ eq "name" (string "Luna"), le "age" (int 14)])
    |> toQueryParameter >> List.singleton >> Url.Builder.toQuery >> Url.percentDecode
    --> Just "?$filter=name eq 'Luna' and age le 14"

-}
and : List CommonExpression -> CommonExpression
and ops =
    And ops


{-| The `or` operator returns `false` if both the left and right operands both evaluate to `false`, otherwise it returns `true`.

The `null` value is treated as unknown, so if one operand evaluates to `null` and the other operand to `true`, the `or` operator returns `true`.
All other combinations with `null` return `null`.

    import Url
    import Url.Builder

    filter (or [ eq "department" (string "Sales"), eq "department" (string "Marketing")])
    |> toQueryParameter >> List.singleton >> Url.Builder.toQuery >> Url.percentDecode
    --> Just "?$filter=department eq 'Sales' or department eq 'Marketing'"

-}
or : List CommonExpression -> CommonExpression
or ops =
    Or ops


{-| The `not` operator returns `true` if the operand returns `false`, otherwise it returns `false`.

The `null` value is treated as unknown, so `not null` returns `null`.

    import Url
    import Url.Builder

    filter (not_ (contains "email" "@org.com"))
    |> toQueryParameter >> List.singleton >> Url.Builder.toQuery >> Url.percentDecode
    --> Just "?$filter=not contains(email,'@org.com')"

-}
not_ : CommonExpression -> CommonExpression
not_ op =
    Not op


{-| The `in` operator returns true if the left operand is a member of the right operand.

The right operand is a comma-separated list of primitive values.

    import Url
    import Url.Builder

    filter (in_ "department" [string "Retail", string "Sales"])
    |> toQueryParameter >> List.singleton >> Url.Builder.toQuery >> Url.percentDecode
    --> Just "?$filter=department in ('Retail', 'Sales')"

-}
in_ : String -> List Value -> CommonExpression
in_ key values =
    In key values



-- String Functions


{-| The `startsWith` function returns `true` if the first string starts with the second string, otherwise it returns `false`

    import Url
    import Url.Builder

    filter (startsWith "CompanyName" "Alfr")
    |> toQueryParameter >> List.singleton >> Url.Builder.toQuery >> Url.percentDecode
    --> Just "?$filter=startswith(CompanyName,'Alfr')"

-}
startsWith : String -> String -> CommonExpression
startsWith key value =
    StartsWith key value


{-| The `endsWith` function returns `true` if the first string ends with the second string, otherwise it returns `false`

    import Url
    import Url.Builder

    filter (endsWith "CompanyName" "Futterkiste")
    |> toQueryParameter >> List.singleton >> Url.Builder.toQuery >> Url.percentDecode
    --> Just "?$filter=endswith(CompanyName,'Futterkiste')"

-}
endsWith : String -> String -> CommonExpression
endsWith key value =
    EndsWith key value


{-| The `contains` function returns `true` if the second string is a substring of the first string, otherwise it returns `false`

    import Url
    import Url.Builder

    filter (contains "CompanyName" "Alfreds")
    |> toQueryParameter >> List.singleton >> Url.Builder.toQuery >> Url.percentDecode
    --> Just "?$filter=contains(CompanyName,'Alfreds')"

-}
contains : String -> String -> CommonExpression
contains key value =
    Contains key value


{-| The `any` operator applies a Boolean expression to each member of a collection and returns `true`
if the expression is `true` for _any_ member of the collection, otherwise it returns `false`.
The `any` operator without an argument returns `true` if the collection is not empty.

    import Url
    import Url.Builder

    filter (any "Items" (gt "Quantity" (int 100)))
    |> toQueryParameter >> List.singleton >> Url.Builder.toQuery >> Url.percentDecode
    --> Just "?$filter=Items/any(a:a/Quantity gt 100)"

-}
any : String -> CommonExpression -> CommonExpression
any key op =
    Any key op


{-| The `all` operator applies a Boolean expression to each member of a collection and returns `true`
if the expression is `true` for _all_ members of the collection, otherwise it returns `false`.

    import Url
    import Url.Builder

    filter (all "Items" (gt "Quantity" (int 100)))
    |> toQueryParameter >> List.singleton >> Url.Builder.toQuery >> Url.percentDecode
    --> Just "?$filter=Items/all(a:a/Quantity gt 100)"

-}
all : String -> CommonExpression -> CommonExpression
all key op =
    All key op


{-| `plainStringOperator` is useful

  - when your target API offers OData operator that is out of the standard scope

  - when your target API OData formatting differs from standard formatting

  - for OData operators that are not implemented by this package

In the following example, `concat` is not implemented in this package.

    import Url
    import Url.Builder

    filter
        ( or
            [ eq "Country" (string "France")
            , plainStringOperator "concat(concat(City,', '), Country) eq 'Berlin, Germany'"
            ]
        )
    |> toQueryParameter >> List.singleton >> Url.Builder.toQuery >> Url.percentDecode
    --> Just "?$filter=Country eq 'France' or concat(concat(City,', '), Country) eq 'Berlin, Germany'"

-}
plainStringOperator : String -> CommonExpression
plainStringOperator s =
    PlainString s


encodeOperator : CommonExpression -> String
encodeOperator operator =
    case operator of
        -- Logical Operators
        EQ key value ->
            key ++ " eq " ++ stringFromValue value

        NE key value ->
            key ++ " ne " ++ stringFromValue value

        GT key value ->
            key ++ " gt " ++ stringFromValue value

        GE key value ->
            key ++ " ge " ++ stringFromValue value

        LT key value ->
            key ++ " lt " ++ stringFromValue value

        LE key value ->
            key ++ " le " ++ stringFromValue value

        In key values ->
            key ++ " in (" ++ String.join ", " (List.map stringFromValue values) ++ ")"

        And ops ->
            String.join " and " (List.map encodeOperator ops)

        Or ops ->
            String.join " or " (List.map encodeOperator ops)

        Not op ->
            "not " ++ encodeOperator op

        -- String Functions
        StartsWith key stringValue ->
            "startswith(" ++ key ++ ",'" ++ stringValue ++ "')"

        EndsWith key stringValue ->
            "endswith(" ++ key ++ ",'" ++ stringValue ++ "')"

        Contains key stringValue ->
            "contains(" ++ key ++ ",'" ++ stringValue ++ "')"

        Any key op ->
            key ++ "/any(a:a/" ++ encodeOperator op ++ ")"

        All key op ->
            key ++ "/all(a:a/" ++ encodeOperator op ++ ")"

        PlainString s ->
            s



-- Value


{-| `Value` type represents [primitive literals](http://docs.oasis-open.org/odata/odata/v4.01/odata-v4.01-part2-url-conventions.html#sec_PrimitiveLiterals)

    NullValue eq null
    TrueValue eq true
    FalseValue eq false
    IntegerValue lt -128
    FloatValue eq 34.95
    StringValue eq 'Say Hello,then go'
    DateValue eq 2012-12-03
    DateTimeOffsetValue eq 2012-12-03T07:16:23
    GuidValue eq 01234567-89ab-cdef-0123-456789abcdef

-}
type Value
    = NullValue
    | TrueValue
    | FalseValue
    | IntValue Int
    | FloatValue Float
    | StringValue String
    | DateValue Time.Posix
    | DateTimeOffsetValue Time.Posix
    | GuidValue String
    | CustomValue String


stringFromValue : Value -> String
stringFromValue v =
    case v of
        NullValue ->
            "null"

        TrueValue ->
            "true"

        FalseValue ->
            "false"

        IntValue i ->
            String.fromInt i

        FloatValue f ->
            String.fromFloat f

        StringValue s ->
            "'" ++ s ++ "'"

        DateValue posix ->
            OData4.Internals.dateStringFromPosix posix

        DateTimeOffsetValue posix ->
            OData4.Internals.dateTimeOffsetStringFromPosix posix

        GuidValue s ->
            s

        CustomValue s ->
            s


{-|

    null
    |> test.stringFromValue
    --> "null"

-}
null : Value
null =
    NullValue


{-|

    true
    |> test.stringFromValue
    --> "true"

-}
true : Value
true =
    TrueValue


{-|

    false
    |> test.stringFromValue
    --> "false"

-}
false : Value
false =
    FalseValue


{-|

    int -128
    |> test.stringFromValue
    --> "-128"

-}
int : Int -> Value
int i =
    IntValue i


{-|

    float 0.31415
    |> test.stringFromValue
    --> "0.31415"

-}
float : Float -> Value
float f =
    FloatValue f


{-| _Note the single quotes_

    string "Say Hello,then go"
    |> test.stringFromValue
    --> "'Say Hello,then go'"

-}
string : String -> Value
string s =
    StringValue s


{-|

    import Time

    date (Time.millisToPosix 1631124861000)
    |> test.stringFromValue
    --> "2021-09-08"

-}
date : Time.Posix -> Value
date posix =
    DateValue posix


{-|

    import Time

    dateTime (Time.millisToPosix 1631124861000)
    |> test.stringFromValue
    --> "2021-09-08T18:14:21Z"

-}
dateTime : Time.Posix -> Value
dateTime posix =
    DateTimeOffsetValue posix


{-|

    guid "01234567-89ab-cdef-0123-456789abcdef"
    |> test.stringFromValue
    --> "01234567-89ab-cdef-0123-456789abcdef"

-}
guid : String -> Value
guid s =
    GuidValue s


{-| In situations like this <https://github.com/microsoftgraph/microsoft-graph-docs/issues/14547>,
custom value may come in handy.
In the example below, date needs to be treated as string (with single quotes):

    import Time

    dateAsString : Time.Posix -> Value
    dateAsString posix =
        customValue (\s -> "'" ++ s ++ "'") (date posix)

    dateAsString (Time.millisToPosix 1631124861000)
    |> test.stringFromValue
    --> "'2021-09-08'"

-}
customValue : (String -> String) -> Value -> Value
customValue f v =
    CustomValue (f (stringFromValue v))



-- Building


{-| Build [`Url.Builder.QueryParameter`](https://package.elm-lang.org/packages/elm/url/latest/Url-Builder#QueryParameter) from [`QueryOption`](#QueryOption),
which you can further combine with other `QueryParameter`s and convert to string using
[`Url.Builder.toQuery`](https://package.elm-lang.org/packages/elm/url/latest/Url-Builder#toQuery)

    import Url
    import Url.Builder

    [ toQueryParameter (top 10) ]
    |> Url.Builder.toQuery >> Url.percentDecode
    --> Just "?$top=10"

-}
toQueryParameter : QueryOption -> Url.Builder.QueryParameter
toQueryParameter queryParameter =
    case queryParameter of
        Select attrs ->
            Url.Builder.string "$select" (String.join "," attrs)

        Filter ops ->
            Url.Builder.string "$filter" (encodeOperator ops)

        OrderBy attrs ->
            Url.Builder.string "$orderBy"
                (List.map
                    (\( attr, maybeOrder ) ->
                        case maybeOrder of
                            Just order ->
                                attr ++ " " ++ stringFromOrder order

                            Nothing ->
                                attr
                    )
                    attrs
                    |> String.join ","
                )

        Top i ->
            Url.Builder.int "$top" i

        Skip i ->
            Url.Builder.int "$skip" i

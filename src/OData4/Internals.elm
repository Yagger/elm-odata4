module OData4.Internals exposing (..)

import Time



-- Date and Time


{-|

    import Time

    Time.millisToPosix 1631124861000
    |> dateStringFromPosix
    --> "2021-09-08"

-}
dateStringFromPosix : Time.Posix -> String
dateStringFromPosix posix =
    String.fromInt (Time.toYear Time.utc posix)
        ++ "-"
        ++ (case Time.toMonth Time.utc posix of
                Time.Jan ->
                    "01"

                Time.Feb ->
                    "02"

                Time.Mar ->
                    "03"

                Time.Apr ->
                    "04"

                Time.May ->
                    "05"

                Time.Jun ->
                    "06"

                Time.Jul ->
                    "07"

                Time.Aug ->
                    "08"

                Time.Sep ->
                    "09"

                Time.Oct ->
                    "10"

                Time.Nov ->
                    "11"

                Time.Dec ->
                    "12"
           )
        ++ "-"
        ++ fromIntWithZeroPadding (Time.toDay Time.utc posix)


{-|

    import Time

    Time.millisToPosix 1631124861000
    |> dateTimeOffsetStringFromPosix
    --> "2021-09-08T18:14:21Z"

-}
dateTimeOffsetStringFromPosix : Time.Posix -> String
dateTimeOffsetStringFromPosix posix =
    dateStringFromPosix posix
        ++ "T"
        ++ fromIntWithZeroPadding (Time.toHour Time.utc posix)
        ++ ":"
        ++ fromIntWithZeroPadding (Time.toMinute Time.utc posix)
        ++ ":"
        ++ fromIntWithZeroPadding (Time.toSecond Time.utc posix)
        ++ "Z"


{-|

    fromIntWithZeroPadding 0
    --> "00"

    fromIntWithZeroPadding 5
    --> "05"

    fromIntWithZeroPadding 15
    --> "15"

    fromIntWithZeroPadding 2021
    --> "2021"

-}
fromIntWithZeroPadding : Int -> String
fromIntWithZeroPadding i =
    String.padLeft 2 '0' (String.fromInt i)

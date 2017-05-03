## Summary

Implentation of the all the exposed functions in the [List module](http://package.elm-lang.org/packages/elm-lang/core/latest/List) from the [elm programming language](http://elm-lang.org/). This was written from scracth just for learning purposes and fun:

```elm
module Fun
    exposing
        ( isEmpty
        , length
        , reverse
        , member
        , head
        , tail
        , filter
        , take
        , drop
        , singleton
        , repeat
        , range
        , (:::)
        , append
        , concat
        , intersperse
        , partition
        , unzip
        , map
        , map2
        , map3
        , filterMap
        , concatMap
        , indexedMap
        , foldr
        , foldl
        , sum
        , product
        , maximum
        , minimum
        , all
        , any
        , scanl
        , sort
        , sortBy
        )

-- BASICS


isEmpty : List a -> Bool
isEmpty list =
    case list of
        [] ->
            True

        _ ->
            False


length : List a -> Int
length list =
    case list of
        hd :: tail ->
            1 + length tail

        [] ->
            0


reverse : List a -> List a
reverse list =
    case list of
        head :: tail ->
            (reverse tail) ++ [ head ]

        [] ->
            []


member : a -> List a -> Bool
member value list =
    case list of
        head :: tail ->
            if value == head then
                True
            else
                member value tail

        [] ->
            False



-- SUB-LISTS


head : List a -> Maybe a
head list =
    case list of
        hd :: _ ->
            Just hd

        [] ->
            Nothing


tail : List a -> Maybe (List a)
tail list =
    case list of
        _ :: tail ->
            Just tail

        [] ->
            Nothing


filter : (a -> Bool) -> List a -> List a
filter func list =
    case list of
        [] ->
            []

        head :: tail ->
            if func head == True then
                head :: filter func tail
            else
                filter func tail


take : Int -> List a -> List a
take n list =
    case list of
        head :: tail ->
            if n == 0 then
                []
            else
                head :: (take (n - 1) tail)

        [] ->
            []


drop : Int -> List a -> List a
drop n list =
    case list of
        head :: tail ->
            if n == 0 then
                list
            else
                drop (n - 1) tail

        [] ->
            []



-- PUTTING LISTS TOGETHER


singleton : a -> List a
singleton value =
    [ value ]


repeat : Int -> a -> List a
repeat n value =
    case n of
        0 ->
            []

        _ ->
            if n == 0 then
                []
            else
                value :: (repeat (n - 1) value)


range : Int -> Int -> List Int
range from to =
    if to - from >= 0 then
        from :: range (from + 1) to
    else
        []


(:::) : a -> List a -> List a
(:::) head tail =
    [ head ] ++ tail


append : List a -> List a -> List a
append list1 list2 =
    list1 ++ list2


concat : List (List a) -> List a
concat list =
    case list of
        [] ->
            []

        head :: tail ->
            head ++ concat tail


intersperse : a -> List a -> List a
intersperse value list =
    case list of
        [] ->
            []

        head :: [] ->
            [ head ]

        head :: tail ->
            [ head ] ++ [ value ] ++ intersperse value tail



-- TAKING LISTS APART


partition : (a -> Bool) -> List a -> ( List a, List a )
partition func list =
    case list of
        [] ->
            ( [], [] )

        head :: tail ->
            ( filter func list, filter (not << func) list )


unzip : List ( a, b ) -> ( List a, List b )
unzip list =
    foldl
        (\( a, b ) ( listA, listB ) ->
            ( listA ++ [ a ], listB ++ [ b ] )
        )
        ( [], [] )
        list



-- MAPPING


map : (a -> b) -> List a -> List b
map func list =
    case list of
        [] ->
            []

        head :: tail ->
            func head :: map func tail


map2 : (a -> b -> c) -> List a -> List b -> List c
map2 func list1 list2 =
    case ( list1, list2 ) of
        ( [], [] ) ->
            []

        ( _, [] ) ->
            []

        ( [], _ ) ->
            []

        ( head1 :: tail1, head2 :: tail2 ) ->
            func head1 head2 :: map2 func tail1 tail2


map3 : (a -> b -> c -> d) -> List a -> List b -> List c -> List d
map3 func list1 list2 list3 =
    case ( list1, list2, list3 ) of
        ( [], [], [] ) ->
            []

        ( _, [], [] ) ->
            []

        ( _, _, [] ) ->
            []

        ( [], _, _ ) ->
            []

        ( _, [], _ ) ->
            []

        ( head1 :: tail1, head2 :: tail2, head3 :: tail3 ) ->
            func head1 head2 head3 :: map3 func tail1 tail2 tail3



-- SPECIAL MAPS


filterMap : (a -> Maybe b) -> List a -> List b
filterMap func list =
    case list of
        head :: tail ->
            case func head of
                Just value ->
                    value :: filterMap func tail

                Nothing ->
                    filterMap func tail

        [] ->
            []


concatMap : (a -> List b) -> List a -> List b
concatMap func list =
    concat (map func list)


indexedMap : (Int -> a -> b) -> List a -> List b
indexedMap func list =
    indexedMapHelper func 0 list


indexedMapHelper : (Int -> a -> b) -> Int -> List a -> List b
indexedMapHelper func index list =
    case list of
        head :: tail ->
            func index head :: indexedMapHelper func (index + 1) tail

        [] ->
            []



-- FOLDING


foldl : (a -> b -> b) -> b -> List a -> b
foldl reducer state list =
    case list of
        head :: tail ->
            foldl reducer (reducer head state) tail

        [] ->
            state


foldr : (a -> b -> b) -> b -> List a -> b
foldr reducer state list =
    foldl reducer state (reverse list)



-- SPECIAL FOLDS


sum : List number -> number
sum list =
    foldl (+) 0 list


product : List number -> number
product list =
    foldl (*) 0 list


maximum : List comparable -> Maybe comparable
maximum list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            Just
                (foldl
                    (\value max ->
                        if value > max then
                            value
                        else
                            max
                    )
                    head
                    tail
                )


minimum : List comparable -> Maybe comparable
minimum list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            Just
                (foldl
                    (\value min ->
                        if value < min then
                            value
                        else
                            min
                    )
                    head
                    tail
                )


all : (a -> Bool) -> List a -> Bool
all func list =
    case list of
        head :: tail ->
            if func head == True then
                all func tail
            else
                False

        [] ->
            True


any : (a -> Bool) -> List a -> Bool
any func list =
    case list of
        head :: tail ->
            if func head == True then
                True
            else
                any func tail

        [] ->
            False


scanl : (a -> b -> b) -> b -> List a -> List b
scanl reducer state list =
    reverse <|
        foldl
            (\val acc ->
                case acc of
                    head :: tail ->
                        reducer val head :: acc

                    _ ->
                        []
            )
            [ state ]
            list



-- SORTING


sort : List comparable -> List comparable
sort list =
    case list of
        [] ->
            []

        head :: tail ->
            sort (filter (\val -> val < head) tail)
                ++ [ head ]
                ++ sort (filter (\val -> val > head) tail)


sortBy : (a -> comparable) -> List a -> List a
sortBy func list =
    case list of
        [] ->
            []

        head :: tail ->
            sortBy func (filter (\val -> func val < func head) tail)
                ++ [ head ]
                ++ sortBy func (filter (\val -> func val > func head) tail)

```
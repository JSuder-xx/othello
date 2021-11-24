module Data.List exposing (cartesianProduct)


cartesianProduct : List a -> List ( a, a )
cartesianProduct lst =
    lst |> List.concatMap (\a -> lst |> List.map (\b -> ( a, b )))

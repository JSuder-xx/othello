module Data.Function exposing (Thunk, flip)


type alias Thunk a =
    () -> a


flip : (a -> b -> c) -> (b -> a -> c)
flip f a b =
    f b a

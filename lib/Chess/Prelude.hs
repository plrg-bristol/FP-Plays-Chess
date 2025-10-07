module Chess.Prelude where

infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

infixl 9 .>
(.>) :: (a -> b) -> (b -> c) -> (a -> c)
f .> g = \x -> g (f x)

namespace Fractions

open FractionsLib
open Option

module Main =

    let a = stringToFraction "1/2"
    let b = stringToFraction "2/3"

    Sum a b |> toString |> printfn "%s"
    Subtract a b |> toString |> printfn "%s"
    Multiply a b |> toString |> printfn "%s"
    Divide a b |> toString |> printfn "%s"

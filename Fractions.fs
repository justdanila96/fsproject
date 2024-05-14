namespace Fractions

type Fraction = { Num: int; Denom: int }

open System.Text.RegularExpressions

module FractionsLib =

    let private tryConvertToInt (i: string) =
        let mutable res = 0
        if System.Int32.TryParse(i, &res) then Some res else None

    open Option

    let private checkAllIsSome arr =
        arr
        |> Array.forall (fun item -> isSome item)
        |> (fun test ->
            if test then
                arr |> Array.map (fun item -> item.Value) |> Some
            else
                None)

    let private tryParseInputString (s: string) =

        let containsTwoItems arr =
            let length = Array.length arr

            match length with
            | 2 -> Some arr
            | _ -> None

        Regex.Split(s, "\/{1}") |> containsTwoItems

    let private tryConvertParsedData arr =
        arr |> Array.map tryConvertToInt |> checkAllIsSome

    let private tryBuildFraction arr =
        match Array.last arr with
        | 0 -> None
        | _ -> { Num = arr[0]; Denom = arr[1] } |> Some

    let stringToFraction str =
        str |> tryParseInputString |> bind tryConvertParsedData |> bind tryBuildFraction

    let private fractionToString fr =

        let isOne a b =
            let absa = abs a
            let absb = abs b
            if absa = absb then "1" else $"{absa}/{absb}"

        match fr with
        | { Fraction.Num = num } when num = 0 -> "0"
        | { Fraction.Num = num
            Fraction.Denom = denom } when num > 0 && denom < 0 || num < 0 && denom > 0 -> "-" + isOne num denom
        | { Fraction.Num = num
            Fraction.Denom = denom } when num > 0 && denom > 0 || num < 0 && denom < 0 -> isOne num denom
        | _ -> "Not a fraction"

    let toString fr =
        match fr with
        | Some f -> fractionToString f
        | None -> "Not a fraction"

    let rec private GCD a b = if a <> 0 then GCD (b % a) a else b

    let private MLC a b = (a * b) / (GCD a b)

    let private reduce fr =
        let gcd = GCD fr.Num fr.Denom

        { Num = fr.Num / gcd
          Denom = fr.Denom / gcd }

    let private applyOpt a b fn = (a, b) ||> map2 fn

    let Sum a b =
        (fun fr1 fr2 ->
            let denom = MLC fr1.Denom fr2.Denom
            let num = denom / fr1.Denom * fr1.Num + denom / fr2.Denom * fr2.Num
            { Num = num; Denom = denom } |> reduce)
        |> applyOpt a b

    let Subtract a b =
        (fun fr1 fr2 ->
            let denom = MLC fr1.Denom fr2.Denom
            let num = denom / fr1.Denom * fr1.Num - denom / fr2.Denom * fr2.Num
            { Num = num; Denom = denom } |> reduce)
        |> applyOpt a b

    let Multiply a b =
        (fun fr1 fr2 ->
            { Num = fr1.Num * fr2.Num
              Denom = fr1.Denom * fr2.Denom }
            |> reduce)
        |> applyOpt a b

    let Divide a b =
        (fun fr1 fr2 ->
            { Num = fr1.Num * fr2.Denom
              Denom = fr1.Denom * fr2.Num }
            |> reduce)
        |> applyOpt a b

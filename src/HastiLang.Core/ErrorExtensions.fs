module ErrorExtensions
open FParsec.Internals
open FParsec

let rep (a:string) b (text:string) = text.Replace(a, b)

let toPersian (text:string) =
    text
    |> rep "The error occured at the end of the line" "خطایی در آخر خط رخ داده است"
    |> rep "Expecting:" "انتظار:"
    |> rep "Note:" "یادداشت:"
    |> rep "Unexpected:" "غیر‌منتظره:"
    |> rep " or " " یا "
    |> rep " and " " و "
    |> rep ", " "، "
    |> rep " could not be parsed because: " " نتوانست تجزیه شود به دلیل: "
    |> rep "The error occurred at the end of the input stream." "خطایی در انتهای ورودی رخ داده است."
    |> rep "Error in Ln:" "خطا در خط:"
    |> rep "Col:" "ستون:"
    |> rep "end of input" "پایان ورودی"
    |> rep "or" "یا"
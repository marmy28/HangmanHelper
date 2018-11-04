namespace HangmanHelper.Program.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit.MsTest
open System

[<TestClass>]
type ProgramTests () =

    [<TestMethod>]
    member __.``removeFromBeginningTildaAndGetProperLength when called with length of 4 and hi``() =
        "~hi" |> Program.removeFromBeginningTildaAndGetProperLength 4 |> String.Concat |> should equal "..hi"

    [<TestMethod>]
    member __.``removeFromEndingTildaAndGetProperLength when called with length of 4 and hi``() =
        "hi~" |> Program.removeFromEndingTildaAndGetProperLength 4 |> String.Concat |> should equal "hi.."

    [<TestMethod>]
    member __.``removeFromMiddleTildaAndGetProperLength when called with length of 4 and hi``() =
        "h~i" |> Program.removeFromMiddleTildaAndGetProperLength 4 |> String.Concat |> should equal "h..i"
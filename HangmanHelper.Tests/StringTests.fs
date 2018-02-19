namespace HangmanHelper.Strings.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open HangmanHelper.Strings
open FsUnit.MsTest

[<TestClass>]
type StringTests () =

    [<TestMethod>]
    member __.``uppercase when called with ai then AI`` () =
        uppercase "ai" |> should equal "AI"

    [<TestMethod>]
    member __.``trim when called with ai and space then ai and no space`` () =
        trim " ai " |> should equal "ai"

    [<TestMethod>]
    member __.``sameLength when called with two strings of the same length then true`` () =
        sameLength "hi" "go" |> should be True

    [<TestMethod>]
    member __.``sameLength when called with two strings of the different length then false`` () =
        sameLength "hello" "go" |> should be False

    [<TestMethod>]
    member __.``sameStart when called with something that starts with something then true`` () =
        sameStart "h" "hello" |> should be True

    [<TestMethod>]
    member __.``sameStart when called with something that does not starts with something then false`` () =
        sameStart "go" "hello" |> should be False

    [<TestMethod>]
    member __.``sameStart when called with something that starts with something flip input then true`` () =
        sameStart "hello" "h" |> should be True

    [<TestMethod>]
    member __.``sameStart when called with something that does not starts with something flip input then false`` () =
        sameStart "hello" "go" |> should be False

    [<TestMethod>]
    member __.``sameEnd when called with something that ends with something then true`` () =
        sameEnd "h" "bath" |> should be True

    [<TestMethod>]
    member __.``sameEnd when called with something that does not ends with something then false`` () =
        sameEnd "go" "bath" |> should be False

    [<TestMethod>]
    member __.``sameEnd when called with something that ends with something flip input then true`` () =
        sameEnd "bath" "h" |> should be True

    [<TestMethod>]
    member __.``sameEnd when called with something that does not ends with something flip input then false`` () =
        sameEnd "bath" "go" |> should be False
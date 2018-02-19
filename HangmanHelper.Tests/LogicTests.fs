namespace HangmanHelper.Logic.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open HangmanHelper.Logic
open FsUnit.MsTest

[<TestClass>]
type LogicTests () =

    [<TestMethod>]
    member __.``distinctLetters when called with a..i and empty string then ai`` () =
        distinctLetters "a..i" "" |> should equal "ai"

    [<TestMethod>]
    member __.``distinctLetters when called with a..i and abc then abci`` () =
        distinctLetters "a..i" "abc" |> should equal "abci"

    [<TestMethod>]
    member __.``getRegexFormat when called with a..i and empty string as a used letter result should be a..i`` () =
        getRegexFormat "a..i" "" |> should equal "a..i"

    [<TestMethod>]
    member __.``getRegexFormat when called with a..i and aei as a used letter result should be a[^aei][^aei]i`` () =
        getRegexFormat "a..i" "aei" |> should equal "a[^aei][^aei]i"

    [<TestMethod>]
    member __.``getMatchedLineCollection when called with only one option that matches should return only that option`` () =
        getMatchedLineCollection "..h." "hj" ["ashs"; "john"] |> should contain "ashs" |> should not' (contain "john")

    [<TestMethod>]
    member __.``mostCommonLetters when called should not contain the already used letters and should have the highest use letter first`` () =
        let actual = mostCommonLetters "hj" ["hello"; "hockey"; "hee"] 
        actual |> Seq.map fst |> should not' (contain 'h')
        actual |> Seq.head |> should equal ('e', 4)
        actual |> Seq.map snd |> should be descending

    [<TestMethod>]
    member __.``getTopThreeMostCommonLetters when called should have length 3`` () =
        let actual = getTopThreeMostCommonLetters "hj" ["hello"; "hockey"; "hee"] 
        actual |> Seq.toArray |> should haveLength 3
        actual |> Seq.map snd |> should be descending

    [<TestMethod>]
    member __.``groupAndSort when called should have length 3`` () =
        let actual = groupAndSort ["hello"; "hockey"; "hee"]
        actual |> Seq.toArray |> should haveLength 1
        actual |> Seq.head |> fst |> should equal 'h'
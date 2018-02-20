module public HangmanHelper.Logic

open System.IO
open System.Text.RegularExpressions
open HangmanHelper.Strings
open System

let getStartingString number = 
    Seq.replicate number '.' |> String.Concat

let distinctLetters s1 s2 =
    s1
    |> Seq.append s2
    |> Seq.distinct
    |> Seq.where ((<>) '.')
    |> Seq.sort
    |> System.String.Concat

let getRegexFormat known alreadyUsed = 
    let replacePeriodString =
        match alreadyUsed with
        | "" -> "."
        | x -> "[^" + x + "]"
    let getCorrectCharacter c =
        match c with
        | '.' -> replacePeriodString
        | x -> Convert.ToString x
    known 
    |> Seq.map getCorrectCharacter
    |> System.String.Concat

let readLines path = File.ReadLines(path)

let isMatch pattern input = Regex.IsMatch(input, pattern)

let getMatchedLineCollection known alreadyUsed lineCollection =
    let modifiedKnownNow = getRegexFormat known alreadyUsed
    lineCollection
    |> Seq.where (sameLength known)
    |> Seq.where (isMatch modifiedKnownNow)

let mostCommonLetters alreadyUsed lineCollection =
    let alreadyGuessed c = alreadyUsed |> Seq.contains c
    lineCollection
    |> Seq.concat
    |> Seq.where (alreadyGuessed >> not)
    |> Seq.groupBy id
    |> Seq.map (fun (key, group) -> (key, Seq.length group))
    |> Seq.sortByDescending snd

let getTopThreeMostCommonLetters alreadyUsed lineCollection =
    lineCollection
    |> mostCommonLetters alreadyUsed
    |> Seq.take 3

let groupAndSort lineCollection =
    lineCollection
    |> Seq.groupBy Seq.head
    |> Seq.sortBy fst

let groupOutput (key, group) =
    let format (index, item) =
        match index % 6 with
        | 0 -> sprintf " %s\n" item
        | _ -> sprintf " %s" item

    let character = sprintf "%c\n" key
    let list = group |> Seq.sort |> Seq.distinct |> Seq.indexed |> Seq.map format |> String.Concat
    character + list + "\n"

let letterAndCountOutput (letter, count) = 
    sprintf "%c count: %d\n" letter count
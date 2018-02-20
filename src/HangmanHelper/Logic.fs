module public HangmanHelper.Logic

open System.IO
open System.Text.RegularExpressions
open HangmanHelper.Strings
open System


///**Description**
/// Gets a string full of periods to start off.
///**Parameters**
///  * `number` - parameter of type `int`. The length of the word.
///**Output Type**
///  * `string` - The length of the word in periods.
let getStartingString number = 
    Seq.replicate number '.' |> String.Concat


///**Description**
/// Getting the distinct letters from two strings excluding periods.
///**Parameters**
///  * `s1` - parameter of type `seq<char>`. The first string.
///  * `s2` - parameter of type `seq<char>`. The second string.
///**Output Type**
///  * `string` - a distinct list of characters.
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
    let getCorrectCharacter =
        function
        | '.' -> replacePeriodString
        | x -> Convert.ToString x
    known 
    |> Seq.map getCorrectCharacter
    |> System.String.Concat

let readLines = File.ReadLines

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
    |> Seq.countBy id
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
        match index % 3 with
        | 0 -> sprintf " %s\n" item
        | _ -> sprintf " %s" item

    let character = sprintf "%c\n" key
    let list = group |> Seq.sort |> Seq.distinct |> Seq.indexed |> Seq.map format |> String.Concat
    character + list + "\n"

let letterAndCountOutput (letter, count) = 
    sprintf "%c count: %d\n" letter count
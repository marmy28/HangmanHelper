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

///**Description**
/// Get the formatted string for regex.
///**Parameters**
///  * `known` - parameter of type `seq<char>`. The string to transform.
///  * `alreadyUsed` - parameter of type `string`. Characters already guessed to not include.
///**Output Type**
///  * `string` - The regex format.
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


///**Description**
/// Reading lines from a file.
///**Parameters**
///  * `arg00` - parameter of type `string`. File name.
///
///**Output Type**
///  * `Collections.Generic.IEnumerable<string>` - lines in the file.
let readLines = File.ReadLines

///**Description**
/// Regex matching.
///**Parameters**
///  * `pattern` - parameter of type `string`. The pattern for which to look.
///  * `input` - parameter of type `string`. The string to look at.
///**Output Type**
///  * `bool` - whether or not the pattern is in the pattern.
let isMatch pattern input = Regex.IsMatch(input, pattern)

///**Description**
/// Gets all the lines that match the input.
///**Parameters**
///  * `known` - parameter of type `string`. The known string.
///  * `alreadyUsed` - parameter of type `string`. The characters already used.
///  * `lineCollection` - parameter of type `seq<string>`. The collection of lines to look through.
///**Output Type**
///  * `seq<string>` - The collection of lines that match the input.
let getMatchedLineCollection known alreadyUsed lineCollection =
    let modifiedKnownNow = getRegexFormat known alreadyUsed
    lineCollection
    |> Seq.where (sameLength known)
    |> Seq.where (isMatch modifiedKnownNow)

///**Description**
/// Gets the most common letters from the line collection
/// excluding the already used.
///**Parameters**
///  * `alreadyUsed` - parameter of type `seq<'a>`. The already used.
///  * `lineCollection` - parameter of type `seq<'b>`. The line collection.
///**Output Type**
///  * `seq<'a * int>` - each letter involved with the number of times it showed up.
/// The most common is at the beginning.
let mostCommonLetters alreadyUsed lineCollection =
    let alreadyGuessed c = alreadyUsed |> Seq.contains c
    lineCollection
    |> Seq.concat
    |> Seq.where (alreadyGuessed >> not)
    |> Seq.countBy id
    |> Seq.sortByDescending snd

///**Description**
/// Returning the top three most common letters.
///**Parameters**
///  * `alreadyUsed` - parameter of type `seq<'a>`. The already used.
///  * `lineCollection` - parameter of type `seq<'b>`. The line collection.
///**Output Type**
///  * `seq<'a * int>` - the top three letters involed with the number of times it showed up.
let getTopThreeMostCommonLetters alreadyUsed lineCollection =
    lineCollection
    |> mostCommonLetters alreadyUsed
    |> Seq.take 3

///**Description**
/// Groups and sorts the line collection by the first character.
///**Parameters**
///  * `lineCollection` - parameter of type `seq<'a>`. The line collection.
///**Output Type**
///  * `seq<'b * seq<'a>>` - Grouped by the first letter and sorting by the first letter.
let groupAndSort lineCollection =
    lineCollection
    |> Seq.groupBy Seq.head
    |> Seq.sortBy fst

///**Description**
/// Gets the output for the group.
///**Parameters**
///  * `key` - parameter of type `char`. The key.
///  * `group` - parameter of type `seq<string>`. The group of words starting with the key.
///**Output Type**
///  * `string` - The output for the group.
let groupOutput (key, group) =
    let format (index, item) =
        match index % 3 with
        | 0 -> sprintf " %s\n" item
        | _ -> sprintf " %s" item

    let character = sprintf "%c\n" key
    let list = group |> Seq.sort |> Seq.distinct |> Seq.indexed |> Seq.map format |> String.Concat
    character + list + "\n"

///**Description**
/// The letter and count output.
///**Parameters**
///  * `letter` - parameter of type `char`. The letter.
///  * `count` - parameter of type `int`. The count of that letter.
///**Output Type**
///  * `string` - the letter and count output.
let letterAndCountOutput (letter, count) = 
    sprintf "%c count: %d\n" letter count
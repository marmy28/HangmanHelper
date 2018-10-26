// Learn more about F# at http://fsharp.org

open HangmanHelper.Logic
open HangmanHelper.Strings
open System

///**Description**
/// The file name.
///**Output Type**
///  * `string` - the file name.
let wordFileName = "Properties/words_alpha.txt"

type UserInput = Yes = 'Y' | No ='N'

///**Description**
/// Transform the user input into what is expected.
/// The call will happen until the user enters the correct input.
///**Parameters**
///  * `func` - parameter of type `string -> Result<'a,string>`. The function to verify user input.
///**Output Type**
///  * `'a` - The user input in the correct format.
let rec transformInput func =
    let input = Console.ReadLine()
    match func input with
    | Result.Ok x -> x
    | Result.Error reason -> printfn "Please give proper input. %s" reason
                             transformInput func

let matchInteger input =
    match System.Int32.TryParse input with
    | (true, number) -> Result.Ok number
    | _ -> Result.Error "Expect a number."

let matchYesOrNo default' input =
    match uppercase input with
    | "Y" -> Result.Ok UserInput.Yes
    | "N" -> Result.Ok UserInput.No
    | "" -> Result.Ok default'
    | _ -> Result.Error "Expect Y or N."

let matchGuessedCharacter default' alreadyGuessed input =
    let isAlreadyInGuessed c =
        match alreadyGuessed |> Seq.contains c with
        | true -> Result.Error "Already guessed that letter."
        | false -> Result.Ok c
    match input |> trim |> System.Char.TryParse with
    | _ when (trim input) = "" -> Result.Ok default'
    | (true, char) -> isAlreadyInGuessed char
    | _ -> Result.Error "Expect a character."

let tryCombine =
    function
    | ('.', '.') -> Some('.')
    | ('.', x) -> Some(x)
    | (x, '.') -> Some(x)
    | (x, y) when x = y -> Some(x)
    | (_, _) -> None

let removeFromBeginningTildaAndGetProperLength l s =
    let sLength = (String.length s) - 1
    let tail = s |> Seq.tail
    Seq.append (Seq.replicate (l - sLength) '.') tail

let removeFromEndingTildaAndGetProperLength l s =
    let sLength = (String.length s) - 1
    let init = s |> Seq.take sLength
    Seq.append init (Seq.replicate (l - sLength) '.')

let matchKnown known input =
    let tilda = Seq.singleton '~'
    let compareFull known input =
        let combinedString = Seq.zip known input |> Seq.map tryCombine
        if (combinedString |> Seq.exists (fun x -> x.IsNone)) then
            Result.Error ("The already known input does not match the current input")
        else
            Result.Ok (combinedString |> Seq.choose id |> System.String.Concat)

    match (sameStart tilda input, sameEnd tilda input, sameLength known input) with
    | (true, true, _) -> Result.Error ("Cannot start and end with '~'.")
    | (true, _, _) -> compareFull known (removeFromBeginningTildaAndGetProperLength (String.length known) input)
    | (_, true, _) -> compareFull known (removeFromEndingTildaAndGetProperLength (String.length known) input)
    | (_, _, true) -> compareFull known input
    | (_, _, _) -> Result.Error ("Does not meet requirements. Use '~' at either the beginning or end to shorten the word.")


let getNewKnown known =
    printfn "To what did it change? (Use '~' at the beginning or ending to shorten the word if you want.)"
    transformInput (matchKnown known)

let rec mainLoop currentlyKnown alreadyGuessedLetters previouslyMatchedLineCollection =
    previouslyMatchedLineCollection 
    |> getTopThreeMostCommonLetters alreadyGuessedLetters 
    |> Seq.iter (letterAndCountOutput >> printf "%s")
    printfn "See words? [y/N]"
    let wantToSeeWords = transformInput (matchYesOrNo UserInput.No)
    match wantToSeeWords with
    | UserInput.Yes -> previouslyMatchedLineCollection |> groupAndSort |> Seq.iter (groupOutput >> printf "%s") |> ignore
    | _ -> ()

    let mostLikelyLetter = previouslyMatchedLineCollection |> getTopThreeMostCommonLetters alreadyGuessedLetters |> Seq.head |> fst
    printfn "What is the next character you tried? (%c)" mostLikelyLetter
    let nextLetter = transformInput (matchGuessedCharacter mostLikelyLetter alreadyGuessedLetters)
    printfn "Did it change the known? [y/N]"
    let changedKnown = transformInput (matchYesOrNo UserInput.No)
    let newKnown = match changedKnown with
                   | UserInput.Yes -> (getNewKnown currentlyKnown)
                   | _ -> currentlyKnown
    let guessed = distinctLetters newKnown (nextLetter |> Seq.singleton |> Seq.append alreadyGuessedLetters)
    printfn "Currently known: %s" newKnown
    printfn "Guessed: %s" guessed
    let matchedLineCollection = previouslyMatchedLineCollection |> getMatchedLineCollection newKnown guessed |> Seq.toList

    match matchedLineCollection with
    | [] -> printf "No more suggestions"
    | [x] -> printf "Only '%s' is left" x
    | _ -> printf "Continue? [Y/n]"
           let wantsToContinue = transformInput (matchYesOrNo UserInput.Yes)
           match wantsToContinue with
           | UserInput.Yes -> mainLoop newKnown guessed matchedLineCollection
           | _ -> printf "Have a good day!"

[<EntryPoint>]
let main _ =
    printfn "How many characters?"
    let startingString = transformInput matchInteger |> getStartingString
    let matchedLineCollection = wordFileName 
                                |> readLines 
                                |> getMatchedLineCollection startingString String.Empty
                                |> Seq.toList
    mainLoop startingString String.Empty matchedLineCollection
    Console.ReadLine() |> ignore
    0 // return an integer exit code
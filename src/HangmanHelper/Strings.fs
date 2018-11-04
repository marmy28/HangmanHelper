module public HangmanHelper.Strings

///**Description**
/// Turns the string into an upper case string.
///**Parameters**
///  * `s` - parameter of type `string`. The string to be upper cased.
///**Output Type**
///  * `string` - Upper case string.
let uppercase (s: string) = s.ToUpperInvariant()

///**Description**
/// Trim the string.
///**Parameters**
///  * `s` - parameter of type `string`. String to be trimmed.
///**Output Type**
///  * `string` - Trimmed string.
let trim (s: string) = s.Trim()

///**Description**
/// Determines if the strings have the same length.
///**Parameters**
///  * `s` - parameter of type `string`. The string to look at the length.
///**Output Type**
///  * `string -> bool` - The function that can test the length of the other string.
let sameLength s = String.length >> (=) (String.length s)

///**Description**
/// Checking if the strings have the same start.
///**Parameters**
///  * `s` - parameter of type `seq<'a>`. The first parameter to check the start.
///**Output Type**
///  * `seq<'a> -> bool` - A function that checks the start of the other string.
let sameStart s = Seq.forall2 (=) s

///**Description**
/// Checking if the strings have the same ending.
///**Parameters**
///  * `s` - parameter of type `seq<'a>`. The first parameter to check the end.
///**Output Type**
///  * `seq<'a> -> bool` - A function that checks the end of the other string.
let sameEnd s = Seq.rev >> (s |> Seq.rev |> sameStart)
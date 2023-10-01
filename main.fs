// F# program to input a string and print out information
// about the # of top 10 letters in that string and substituting certain words
//
// Name: Jimmy Patel
// NetID: jpate289@uic.edu
// UIN: 656971609

//
// explode:
//
// Given a string s, explodes the string into a list of characters.
// Example: explode "apple" => ['a';'p';'p';'l';'e']
//
let explode (S: string) = List.ofArray (S.ToCharArray())

//
// implode
//
// The opposite of explode --- given a list of characters, returns
// the list as a string. Example: implode ['t';'h';'e'] => "the"
//
let implode (L: char list) = new string (List.toArray L)

//
// returns length of the string
//
let rec length L =
    match L with
    | [] -> 0 // Empty list
    | _ :: rest -> 1 + length rest // return the length of the list (recursion used for iterating)

let high_frequency_chars = [ 'e'; 't'; 'a'; 'o'; 'i'; 'n'; 's'; 'r'; 'h'; 'l' ]

//
// returns # for specific letter
//
let rec count_chars chars_list char_to_count =
    match chars_list with
    | [] -> 0
    | head :: tail when head = char_to_count -> 1 + count_chars tail char_to_count // If the head matches the character, increment count.
    | _ :: tail -> count_chars tail char_to_count // continuing with the rest of the list.

//
// returns total # for top 10 letters of the English alphabet in the string
//
let rec topTen L =
    List.fold
        (fun count char ->
            if List.contains char high_frequency_chars then
                count + 1
            else
                count)
        0
        L

//
// returns # for each top 10 letters of the English alphabet in the string
//
let count_each_topTen L =
    List.map (fun char -> (char, count_chars L char)) high_frequency_chars

//
// TODO: substitute a series of letters from a given string
//
let rec make_substitutions subs =
    match subs with
    | 't' :: 'h' :: 'e' :: rest -> 'h' :: 'e' :: 'r' :: make_substitutions rest
    | 'b' :: 'o' :: 'y' :: rest -> 'm' :: 'a' :: 'n' :: make_substitutions rest
    | 'r' :: 'a' :: 't' :: rest -> 'h' :: 'a' :: 't' :: make_substitutions rest
    | head :: tail -> head :: make_substitutions tail
    | [] -> []

[<EntryPoint>]
let main argv =
    printfn "Starting"
    printfn ""

    printf ("input> ")
    let input = System.Console.ReadLine()

    let L = explode input
    printfn "exploded: %A" L

    printfn ""
    let len = length L
    printfn "length of sentence: %A" len

    let num = topTen L
    printfn "# of top 10 letters: %A" num

    //
    // TODO: print count of each of the top 10 letters
    //
    let counts = count_each_topTen L
    List.iter (fun (char, count) -> printfn "\'%c\': %d" char count) counts

    let S = implode L
    printfn "imploded: %A" S
    printfn ""

    //
    // TODO: print updated sentence
    //
    let newStringList = make_substitutions L
    let newString = implode newStringList
    printfn "swap imploded: %A" newString

    printfn ""
    printfn "Done"
    0 // return 0 => success, similar to C++
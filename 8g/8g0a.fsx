type codeColor = Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int
type board = ( code * answer ) list
type player = Human | Computer

//Validate
//
///<summary>
///Tager to lister af formen (T' * int) list, dem som List.CountBy retunere,
///og finder de elementer i b som også er i a, og trækker b tallet fra a tallet.
///</summary>
let minusCountLists a b =
    List.fold (fun acc (elem, count) ->
                try 
                    (elem,count - (snd (List.find (fun (elem2, _) ->
                                                    elem2 = elem) b)))::acc
                with
                    _ -> (elem,count)::acc
                ) [] a
///<summary>
///Tager 2 lister på formen, (T' * int) list, som dem retuneret af
///List.CountBy. Retunere en ny liste på samme form, som indholder de 
///elementer i a som også findes i b, og tallet i tuplen er størrer end 
///0 i begge liste. Hvis elementet findes i begge lister, og tallene er
///størrer end 0, vil tallet i den nye liste være det mindste af de to tal.
///</summary>
let reduceCountLists a b =
    List.fold (fun acc (elem, count) ->
                try 
                    let Cb = snd (List.find (fun (elem2, _) ->
                            elem2 = elem) b)
                    if Cb < 1 || count < 1 then acc 
                    else
                        if Cb < count then (elem, Cb)::acc
                        else (elem, count)::acc 
                with
                    _ -> acc
                ) [] a

///<summary>
///Sammenligner et gæt og svaret, og giver antallet af sorte og hvide pinde
///</summary>
///<params name="answer">
///Det rigtige svar, af typen code som er 4 lang
///</params>
///<params name="guess">
///Gættet, af typen code som er 4 lang
///</params>
///<returns>
///Et answer
///</returns>
let validate answer guess =
//bestem sort
    let blacks = List.fold2 (fun acc elem1 elem2 ->
                            if elem1 = elem2 then elem1::acc
                            else acc) [] answer guess
//find hvid
    let Cblacks = (List.countBy (fun elem -> elem) blacks)
    let Cguess = minusCountLists (List.countBy (fun elem -> elem) guess) Cblacks
    let Cans = minusCountLists (List.countBy (fun elem -> elem) answer) Cblacks
    
    let whites = reduceCountLists Cans Cguess
    let whitesCount = List.fold (fun acc elem -> (snd elem) + acc) 0 whites

    (whitesCount, blacks.Length)
////////////////////////////////////////////////////////////////////////////////
//MakeCode
//
///<summary>
///Funktionen numbToCol, tager et tal og matcher det med en farve.
///</summary>
///<param name="chalNumb">Er tallet der skal laves til en farve.</param>
let numbToCol chalNumb =
    match chalNumb with
    | 0 -> Red
    | 1 -> Green
    | 2 -> Yellow
    | 3 -> Purple
    | 4 -> White
    | 5 -> Black
    | 48 -> Red
    | 49 -> Green
    | 50 -> Yellow
    | 51 -> Purple
    | 52 -> White
    | 53 -> Black
    | _ -> failwith "You gone done fucked up"
///<remarks>Vi har 48 til 53, da det er unicode for 0 til 5, hvilket er nødvendigt når brugeren indtaster deres opgave i terminalen.</remarks>
///<returns>En Color type, eller kaster en undtagelse.</returns>

///<summary>Håndtere inputtet fra spilleren når de skal gætte</summary>
let rec getCodeFromHuman msg = 
    printfn "%s: \n (x,y,z,c) where x,y,z,c are your colors. Write 
    0 for Red 
    1 for Green 
    2 for Yellow 
    3 for Purple 
    4 for White 
    5 for Black" msg
    let input = System.Console.ReadLine()
    try 
        List.init 4 (fun i -> numbToCol(int(input.[2*i+1])))  
    with 
        | _ -> getCodeFromHuman msg

///<remarks>u defineres som System.Random() for at System.Random() kan bruges flere gange i streg uden at give det samme tal.</remarks>
let u = System.Random()

///<summary>Funktionen makeCode lave en opgave/challenge til spilleren eller computeren.</summary>
///<param name="player">parametren er hvor vidt spilleren er et menneske eller en datamat, vha. typen player.</param>
let makeCode player =
    if player = Computer then
        [numbToCol(u.Next(0, 4)); numbToCol(u.Next(0, 4)); numbToCol(u.Next(0, 4)); numbToCol(u.Next(0, 4))]
    else
        getCodeFromHuman "Give your challenge as the following example" 
///<returns>En challenge/opgave af typen 'code'.</returns>


///////////////////////////////////////////////////////////////////////////////
//Guess
//
///<summary>
///Laver en codeColor om til en streng
///</summary>
let string (color:codeColor) : string = 
    match color with
    | Red -> "Red"
    | Green -> "Green"
    | Yellow -> "Yellow"
    | Purple -> "Purple"
    | White -> "White"
    | Black -> "Black"
///<summary>Printer et board til consolen</summary>
let printBoard (gameBoard: board) =
    for i in gameBoard do
        let (guess, result) = i
        printfn "%-8s %-8s %-8s %-8s Whites: %d Blacks: %d" (string(guess.[0])) 
                                                        (string(guess.[1]))
                                                        (string(guess.[2]))
                                                        (string(guess.[3]))
                                                        (fst result) (snd result)
    printfn "-------------------------------------------------------"
///<summary>
///Hvis player er Human får getCodeFromHuman deres gæt, ellers 
///finder computeren ud af det koder der er mulige ud fra dens andre svar, og
///retunere et af dem. Hvis det er første tur retunere computeren koden
///[Red;Red;Green;Green]
///</summary>
///<params name="player">En spiller enten computer eller mennekse</param>
///<params name="gameBoard">
///De gæt og svar der har været indtil videre i spillet
///<params>
///<returns>En kode</returns>
let uguess (S:codeColor list list) player (gameBoard:board) = 
    let removeWrong combs (guess:code) (answer:answer) =
        List.filter (fun comb -> (validate comb guess) = answer) combs
    match player with
    | Human -> getCodeFromHuman "Please enter your guess"
    | Computer when gameBoard.Length > 0 ->
        (List.fold (fun acc elem ->
                     removeWrong acc (fst elem) (snd elem)
        ) S gameBoard).[0]
    | _ -> [Red;Red;Green;Green]

//En liste af alle mulige koder
let mutable S = []
for p = 5 downto 0 do
    for j = 5 downto 0 do
        for k = 5 downto 0 do
            for l = 5 downto 0 do
                S <-  [numbToCol p; numbToCol j; numbToCol k; numbToCol l] :: S
///<remark>
///Opretter guess som opgaven beskriver den skal være, da den lige nu forventer
///en liste af alle mulige kode kombinationere for, at virke
///</remark>
let guess = uguess S

///////////////////////////////////////////////////////////////////////////////
//Main
//
///<summary>
///Får et input fra brugeren, tjekker om det er lovligt, hvis ikke spørger
///den igen
///</summary>
let rec getInput msg validInputs = 
    printfn "%s" msg
    let input = System.Console.ReadLine()
    if (List.exists (fun elem -> elem = input) validInputs) then
        input
    else 
        getInput msg validInputs

///<summary>
///Ændre stregene til typen player
///</summay>
let strToPlayer str = 
    match str with 
    | "H" | "h" -> Human
    | "C" | "c" -> Computer
    | _ -> failwith "Invalid player type"

///<summary>
///Loopen af spillet, stopper når spillet er vundet.
///</summary>
///<parmas name="players">
///Spilleren som skal gætte det rigtige svar
///</params>
///<parmas name="secretCode">
///Koden som skal gættes
///</params>
///<parmas name="gameBoard">
///Alle tidligere gæt og svar
///</params>
///<returns>
///Har ikke nogen return, printer en besked hvis spillet er vundet
///</returns>
let rec gameLoop player secretCode gameBoard = 
    printBoard gameBoard
    let currentGuess = guess player gameBoard
    let answerPins = validate secretCode currentGuess
    match answerPins with
    | (0, 4) -> printfn "The code was guessed in %d turns" (gameBoard.Length)
    | _ -> gameLoop player secretCode ((currentGuess, answerPins)::gameBoard)

///////////////////////////////////////////////////////////////////////////////
//Tests
let runTest() =
//minusCountListsT
    printfn "minusCountLists tests:"
    printfn "input: [(\"1\", 20), (\"2\", 30); (\"4\", 0)], [(\"1\", 7); (\"2\", 10); \
(\"4\", 5)]" 
    printfn "expected output: [(\"4\", -5); (\"2\", 20); (\"1\", 13)]"
    printfn "output:          %A\n" 
        ((minusCountLists [("1", 20); ("2", 30); ("4", 0)] 
            [("1", 7); ("2", 10);("4", 5)]))

    printfn "input: [],[(1, 7); (2, 10); (4, 5)]"
    printfn "expected output: []"
    printfn "output:          %A\n"
        ((minusCountLists [] [("1", 7); ("2", 10);("4", 5)]))

    printfn "input: [], []"
    printfn "expected output: []"
    printfn "output:          %A\n"  (minusCountLists [] [])

    printfn "input: [(Red, 7); (Green, 10); (White, 5)], []"
    printfn "expected output: [(Red, 7); (Green, 10); (White, 5)]"
    printfn "output:          %A\n" 
        (minusCountLists [(Red, 7); (Green, 10); (White, 5)] [])
//reduceCountLists
    printfn "reduceCountLists tests:"
    printfn "input: [(\"1\", 20), (\"2\", 30); (\"4\", 0)], [(\"1\", 7); (\"2\", 10); \
(\"4\", 5)]" 
    printfn "expected output: [(\"2\", 10); (\"1\", 7)]" 
    printfn "output:          %A\n" 
        ((reduceCountLists [("1", 20); ("2", 30); ("4", 0)] 
            [("1", 7); ("2", 10);("4", 5)]))

    printfn "input: [],[(1, 7); (2, 10); (4, 5)]"
    printfn "expected output: []"
    printfn "output:          %A\n"
        ((reduceCountLists [] [("1", 7); ("2", 10);("4", 5)]))

    printfn "input: [], []"
    printfn "expected output: []"
    printfn "output:          %A\n"  (reduceCountLists [] [])

    printfn "input: [(Red, 7); (Green, 10); (White, 5)], []"
    printfn "expected output:  []"
    printfn "output:           %A\n"
        (reduceCountLists [(Red, 7); (Green, 10); (White, 5)] [])

//ValidateT
    printfn "Validate tests:" 
    printfn "[Red; Green; Black; White] , [Red; Green; Black; White]"
    printfn "expected output: (0,4)"
    printfn "output:          %A\n"
             (validate [Red; Green; Black; White] [Red; Green; Black; White])

    printfn "input: [Red; Black; Black; White], [Red; Green; Black; White]"
    printfn "expected output: (0,3)"
    printfn "output:          %A\n"
             (validate [Red; Black; Black; White] [Red; Green; Black; White])
    printfn "input: [Yellow; Black; Black; Black], [Red; Green; Black; Yellow]"
    printfn "expected output: (1,1)"
    printfn "output:          %A\n" 
             (validate [Yellow; Black; Black; Black] [Red; Green; Black; Yellow])
    printfn "input: [Yellow; Green; White; Black], [Black; White; Green; Yellow]"
    printfn "expected output: (4,0)"
    printfn "output:          %A\n" 
             (validate [Yellow; Green; White; Black] [Black; White; Green; Yellow])

//makeCodeT
    printfn "MakeCode tests:"
    printfn "returns a list of 4 codeColours if player is computer: %A" (((makeCode Computer):code).Length = 4)

//GuessT
    printfn "Guess tests:"
    printfn "returns a list of 4 codeColors if player is computer: %A" (((guess Computer []):code).Length = 4)

///////////////////////////////////////////////////////////////////////////////
//Spillets start
printfn "Welcome to Mastermind." 
match getInput "Enter 't' to see tests, or 'g' to play the game" 
                ["t";"g"] with
| "t" -> runTest()
| "g" ->  let players = 
            (strToPlayer (getInput 
                "Is the first player a Human or Computer?(Enter H or C):"
                ["H"; "C"; "h"; "c"]),
                strToPlayer (getInput 
                        "And the second player (Enter H or C):" 
                        ["H"; "C"; "h"; "c"])
            )
          let secretCode = makeCode (fst players) 
          gameLoop (snd players) secretCode []
| _ -> failwith "Got wrong input, getInput is broken."

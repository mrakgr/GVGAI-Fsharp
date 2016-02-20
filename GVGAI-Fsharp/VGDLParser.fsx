// TODO: Replace all the lists in here with maps. I fucked this up.
// Had I done this in the first place, the semantic phase would have been so much easier.

module VGDLParser
// The first step of the compilation - the parser for the raw VGDL specifications. 
// Creates a bare bones abstract syntax tree for the VGDL language from a text string.
#nowarn "667"

#if INTERACTIVE
#r "../packages/MonoGame.Framework.WindowsDX.3.4.0.459/lib/net40/MonoGame.Framework.dll"
#r "../packages/FParsec.1.0.2/lib/net40-client/FParsecCS.dll"
#r "../packages/FParsec.1.0.2/lib/net40-client/FParsec.dll"
#endif

open Microsoft.Xna.Framework
    
open FParsec
open System.Collections.Generic

type MainClassTypes =
| NoClass
| Immovable
| FlakAvatar
| Bomber
| Spawnpoint
| Missile
| ShootAvatar
| RandomNPC
| Flicker
| Door
| Resource
| MovingAvatar

type PhysicsTypes =
| GridPhysics

type AttributeTypes =
| MainClass of MainClassTypes
// Attributes
| ShootType of string
| Cooldown of int
| Probability of float
| Orientation of float32 * float32
| Color of uint8 * uint8 * uint8
| Speed of float
| Image of string
| Total of int
| Singleton of bool
| ShrinkFactor of float
| Limit of int
| PhysicsType of PhysicsTypes

type AttributeTypesRecord =
    {
    mclass : MainClassTypes
    shootType : int
    cooldown : int
    probability : float
    orientation : float32 * float32
    color : uint8 * uint8 * uint8
    speed : float
    image : string
    total : int
    singleton : bool
    shrinkfactor : float
    limit : int
    }

type GameEndTypes = Continue | Win | Lose

type TerminationTypes =
| TerminationLimit of int
| TerminationWin of GameEndTypes
| TerminationSType of string
| TerminationNumberedSType of int * string

type TerminationSetTypes =
| Timeout of HashSet<TerminationTypes>
| SpriteCounter of HashSet<TerminationTypes>
| MultiSpriteCounter of HashSet<TerminationTypes>

type TerminationSetTaggedTypes =
| TimeoutTagged of int * GameEndTypes
| SpriteCounterTagged of int * int * GameEndTypes
| MultiSpriteCounterTagged of int [] * int * GameEndTypes

type SpriteSetTypes =
| Sprite of string * HashSet<AttributeTypes>
| SubSprites of SpriteSetTypes list

type InteractionTypesDelayed =
| StepBack

type InteractionTypesImmediate =
| TurnAround
| ScoreChange of int
| CollectResource of int
| KillIfFromAbove of int
| KillIfOtherHasMore of string * int
| KillIfOtherHasMoreTagged of int * int
| KillSprite
| TransformTo of string
| TransformToTagged of int
| CloneSprite

type InteractionTypes =
| DelayedInteraction of InteractionTypesDelayed
| ImmediateInteraction of InteractionTypesImmediate

type InteractionSetTypes =
| Interaction of (string * string) * InteractionTypes list
| TaggedInteraction of (int [] * int []) * InteractionTypes list
| TaggedInteractionEOS of int [] * InteractionTypes list

type SpriteParserTypes =
| SpriteSet of SpriteSetTypes list
| TerminationSet of TerminationSetTypes list
| InteractionSet of InteractionSetTypes list
| LevelMappingSet of Map<char,string>


module ColorsConstants =
    let GREEN = (0uy,200uy,0uy)
    let BLUE = (0uy,0uy,200uy)
    let RED = (200uy,0uy,0uy)
    let GRAY = (90uy,90uy,90uy)
    let WHITE = (250uy,250uy,250uy)
    let BROWN = (140uy,120uy,100uy)
    let BLACK = (0uy,0uy,0uy)
    let ORANGE = (250uy,160uy,0uy)
    let YELLOW = (250uy,250uy,0uy)
    let PINK = (250uy,200uy,200uy)
    let GOLD = (250uy,212uy,0uy)
    let LIGHTRED = (250uy,50uy,50uy)
    let LIGHTORANGE = (250uy,200uy,100uy)
    let LIGHTBLUE = (50uy,100uy,250uy)
    let LIGHTGREEN = (50uy,250uy,50uy)
    let LIGHTGRAY = (150uy,150uy,150uy)
    let DARKGRAY = (30uy,30uy,30uy)
    let DARKBLUE = (20uy,20uy,100uy)

    let color_parsers = 
        let GREEN_parser = attempt (pstringCI "GREEN" |>> (fun x -> Color GREEN))
        let BLUE_parser = attempt (pstringCI "BLUE" |>> (fun x -> Color BLUE))
        let RED_parser = attempt (pstringCI "RED" |>> (fun x -> Color RED))
        let GRAY_parser = attempt (pstringCI "GRAY" |>> (fun x -> Color GRAY))
        let WHITE_parser = attempt (pstringCI "WHITE" |>> (fun x -> Color WHITE))
        let BROWN_parser = attempt (pstringCI "BROWN" |>> (fun x -> Color BROWN))
        let BLACK_parser = attempt (pstringCI "BLACK" |>> (fun x -> Color BLACK))
        let ORANGE_parser = attempt (pstringCI "ORANGE" |>> (fun x -> Color ORANGE))
        let YELLOW_parser = attempt (pstringCI "YELLOW" |>> (fun x -> Color YELLOW))
        let PINK_parser = attempt (pstringCI "PINK" |>> (fun x -> Color PINK))
        let GOLD_parser = attempt (pstringCI "GOLD" |>> (fun x -> Color GOLD))
        let LIGHTRED_parser = attempt (pstringCI "LIGHTRED" |>> (fun x -> Color LIGHTRED))
        let LIGHTORANGE_parser = attempt (pstringCI "LIGHTORANGE" |>> (fun x -> Color LIGHTORANGE))
        let LIGHTBLUE_parser = attempt (pstringCI "LIGHTBLUE" |>> (fun x -> Color LIGHTBLUE))
        let LIGHTGREEN_parser = attempt (pstringCI "LIGHTGREEN" |>> (fun x -> Color LIGHTGREEN))
        let LIGHTGRAY_parser = attempt (pstringCI "LIGHTGRAY" |>> (fun x -> Color LIGHTGRAY))
        let DARKGRAY_parser = attempt (pstringCI "DARKGRAY" |>> (fun x -> Color DARKGRAY))
        let DARKBLUE_parser = attempt (pstringCI "DARKBLUE" |>> (fun x -> Color DARKBLUE))

        choice [|GREEN_parser;BLUE_parser;RED_parser;GRAY_parser;WHITE_parser;BROWN_parser;BLACK_parser;ORANGE_parser;YELLOW_parser;PINK_parser;GOLD_parser;LIGHTRED_parser;LIGHTORANGE_parser;LIGHTBLUE_parser;LIGHTGREEN_parser;LIGHTGRAY_parser;DARKGRAY_parser;DARKBLUE_parser|]

module OrientationConstants =
    let UP = (0.0f, -1.0f)
    let DOWN = (0.0f, 1.0f)
    let LEFT = (-1.0f, 0.0f)
    let RIGHT = (1.0f, 0.0f)
    let DOWN_RIGHT = (1.0f, 1.0f)

    let orientation_parsers =
        let UP_parser = attempt (pstringCI "UP" |>> (fun x -> Orientation UP))
        let DOWN_parser = attempt (pstringCI "DOWN" |>> (fun x -> Orientation DOWN))
        let LEFT_parser = attempt (pstringCI "LEFT" |>> (fun x -> Orientation LEFT))
        let RIGHT_parser = attempt (pstringCI "RIGHT" |>> (fun x -> Orientation RIGHT))

        choice [|UP_parser;DOWN_parser;LEFT_parser;RIGHT_parser|]


module Inner =
    type ExpectationEnum =
    | SAME = 0
    | SAME_UP = 1

    let rec many1Indents (same: Parser<_,_>) (up: Parser<_,_>) (stream: CharStream<_>) =
        let getIndent() =
            let indent = stream.SkipNewlineThenWhitespace(4,false) // Returns -1 if not at newline.
            if indent = -1 then stream.Column-1L |> int else indent // If at newline skip it, else set indent to column.

        let indent = getIndent()
        let result = same stream
        if result.Status = Ok then
            let rec loop acc expectation =
                let indent2 = getIndent()
            
                if not stream.IsEndOfStream then
                    if indent2 > indent then
                        if expectation = ExpectationEnum.SAME_UP then
                            let result = up stream
                            if result.Status = Ok then loop (result.Result::acc) ExpectationEnum.SAME 
                            else Reply(result.Status,result.Error)
                        else Reply(Error, messageError "child indents not aligned")
                    else if indent2 = indent then
                        let result = same stream
                        if result.Status = Ok then loop (result.Result::acc) ExpectationEnum.SAME_UP else Reply(result.Status,result.Error)
                    else Reply(acc |> List.rev)
                else Reply(acc |> List.rev)
            loop [result.Result] ExpectationEnum.SAME_UP
        else Reply(result.Status,result.Error)

    let resultSatisfies predicate msg (p: Parser<_,_>) : Parser<_,_> =
        let error = messageError msg
        fun stream ->
            let state = stream.State
            let reply = p stream
            if reply.Status <> Ok || predicate reply.Result then reply
            else
                stream.BacktrackTo(state) // backtrack to beginning
                Reply(Error, error)

    let resultChoose predicate msg (p: Parser<_,_>) : Parser<_,_> =
        let error = messageError msg
        fun stream ->
            let state = stream.State
            let reply = p stream
            if reply.Status = Ok then
                match predicate reply.Result with
                | Some x -> Reply(x)
                | None -> 
                    stream.BacktrackTo(state) // backtrack to beginning
                    Reply(Error, error)
            else
                stream.BacktrackTo(state) // backtrack to beginning
                Reply(Error, error)

    let blanks = skipManySatisfy <| isAnyOf " "
    let word = many1SatisfyL isAsciiLetter "word"
    let pbool = stringCIReturn "true" true <|> stringCIReturn "false" false

    let sprite_start = word .>> (blanks >>. skipChar '>' >>. blanks) |>> (fun x -> x.ToLower())

    let attribute = 
        choiceL [|    
            attempt (skipStringCI "Immovable" >>. blanks |>> (fun _ -> MainClass Immovable));
            attempt (skipStringCI "FlakAvatar" >>. blanks |>> (fun _ -> MainClass FlakAvatar));
            attempt (skipStringCI "Bomber" >>. blanks |>> (fun _ -> MainClass Bomber));
            attempt (skipStringCI "Spawnpoint" >>. blanks |>> (fun _ -> MainClass Spawnpoint));
            attempt (skipStringCI "Missile" >>. blanks |>> (fun _ -> MainClass Missile));
            attempt (skipStringCI "ShootAvatar" >>. blanks |>> (fun _ -> MainClass ShootAvatar));
            attempt (skipStringCI "RandomNPC" >>. blanks |>> (fun _ -> MainClass RandomNPC));
            attempt (skipStringCI "Flicker" >>. blanks |>> (fun _ -> MainClass Flicker));
            attempt (skipStringCI "Resource" >>. blanks |>> (fun _ -> MainClass Resource));
            attempt (skipStringCI "Door" >>. blanks |>> (fun _ -> MainClass Door));
            attempt (skipStringCI "MovingAvatar" >>. blanks |>> (fun _ -> MainClass MovingAvatar));

            attempt (skipStringCI "stype" >>. blanks >>. skipChar '=' >>. blanks >>. word .>> blanks |>> (fun x -> ShootType x));
            attempt (skipStringCI "cooldown" >>. blanks >>. skipChar '=' >>. blanks >>. pint32 .>> blanks |>> (fun x -> Cooldown x));
            attempt (skipStringCI "prob" >>. blanks >>. skipChar '=' >>. blanks >>. pfloat .>> blanks |>> (fun x -> Probability x));
            attempt (skipStringCI "orientation" >>. blanks >>. skipChar '=' >>. blanks >>. OrientationConstants.orientation_parsers) .>> blanks ;
            attempt (skipStringCI "color" >>. blanks >>. skipChar '=' >>. blanks >>. ColorsConstants.color_parsers) .>> blanks ;
            attempt (skipStringCI "speed" >>. blanks >>. skipChar '=' >>. blanks >>. pfloat .>> blanks |>> (fun x -> Speed x));
            attempt (skipStringCI "img" >>. blanks >>. skipChar '=' >>. blanks >>. (word .>> optional (pstring ".png")) .>> blanks |>> (fun x -> Image x));
            attempt (skipStringCI "total" >>. blanks >>. skipChar '=' >>. blanks >>. pint32 .>> blanks |>> (fun x -> Total x));
            attempt (skipStringCI "singleton" >>. blanks >>. skipChar '=' >>. blanks >>. pbool .>> blanks |>> (fun x -> Singleton x));
            attempt (skipStringCI "shrinkfactor" >>. blanks >>. skipChar '=' >>. blanks >>. pfloat .>> blanks |>> (fun x -> ShrinkFactor x));
            attempt (skipStringCI "limit" >>. blanks >>. skipChar '=' >>. blanks >>. pint32 .>> blanks |>> (fun x -> Limit x));
            attempt (skipStringCI "physicstype" >>. blanks >>. skipChar '=' >>. blanks >>. 
                (choice 
                    [|skipStringCI "GridPhysics" .>> blanks |>> (fun x -> PhysicsType GridPhysics)|]));
            |] "attribute"
                
    let nl = (newline |>> ignore) <|> eof

    let sprite =
        let distinction_func = 
            function
            | MainClass _ -> 0
            // Only one main class.
            | ShootType _ -> 1
            | Cooldown _ -> 2
            | Probability _ -> 3
            | Orientation _ -> 4
            | Color _ -> 5
            | Speed _ -> 6
            | Image _ -> 7
            | Total _ -> 8
            | Singleton _ -> 9
            | ShrinkFactor _ -> 10
            | Limit _ -> 11
            | PhysicsType _ -> 12

        let manyAttrs =
            manyTill attribute nl
            |> resultChoose (
                fun x ->
                    let t = HashSet(x,HashIdentity.FromFunctions distinction_func (fun x y -> true))
                    if t.Count = x.Length then Some t else None
                ) "duplicate detected."

        pipe4 sprite_start blanks manyAttrs blanks (fun a b c _ -> Sprite(a,c))
        
    let sprites = 
        let rec y f x = f (y f) x // The Y Combinator
        //let sprites_up = many1Indents sprite sprites_up |>> (fun x -> SubSprites x)
        let sprites_up = y (fun f -> many1Indents sprite f |>> (fun x -> SubSprites x))
        many1Indents sprite sprites_up

    let wrong_indent = (fun _ -> Reply(Error, messageError "child indents not aligned")) 

    let level_mapping = pipe3 anyChar (blanks >>. (skipChar '>') >>. blanks) (word .>> blanks) (fun a b c -> a, c)
    let level_mappings = 
        many1Indents level_mapping wrong_indent 
        |> resultChoose
            (fun x ->
            let t = Map(x)
            if t.Count = x.Length then Some t else None) "duplicate entries in LevelMapping"
        |> resultSatisfies (fun m -> not <| m.ContainsKey 'w') "w cannot be reassigned from wall in LevelMapping"
        |>> fun x -> x.Add('w',"wall")
        |> resultSatisfies (fun m -> not <| m.ContainsKey 'A') "A cannot be reassigned from avatar in LevelMapping"
        |>> fun x -> x.Add('A',"avatar")

    let to_gameendtype =
        function
        | true -> Win
        | false -> Lose

    let termination_choices = 
        let timeout_term = 
            let timeout_start = skipStringCI "timeout" >>. blanks
            let limit = skipStringCI "limit" >>. blanks >>. skipChar '=' >>. blanks >>. pint32 .>> blanks |>> (fun x -> TerminationLimit x)
            let win = pstringCI "win" >>. blanks >>. skipChar '=' >>. blanks >>. pbool .>> blanks |>> (fun x -> TerminationWin <| to_gameendtype x)
            timeout_start >>. manyTill (choiceL [|limit;win|] "limit or win for timeout") nl .>> spaces
            |> resultChoose (
                fun x ->
                    let df =
                        function
                        | TerminationLimit _ -> 0
                        | TerminationWin _ -> 1
                        | _ -> failwith "Can't touch this."
                    let t = HashSet(x,HashIdentity.FromFunctions df (fun x y -> true))
                    if t.Count = x.Length then Some t else None
                ) "duplicates detected"
            |>> Timeout

        let spritecounter_term =
            let spritecounter_start = skipStringCI "SpriteCounter" >>. blanks
            let stype = skipStringCI "stype" >>. blanks >>. skipChar '=' >>. blanks >>. word .>> blanks |>> (fun x -> TerminationSType x)
            let limit = skipStringCI "limit" >>. blanks >>. skipChar '=' >>. blanks >>. pint32 .>> blanks |>> (fun x -> TerminationLimit x)
            let win = pstringCI "win" >>. blanks >>. skipChar '=' >>. blanks >>. pbool .>> blanks |>> (fun x -> TerminationWin <| to_gameendtype x)
            spritecounter_start >>. manyTill (choice [|stype;limit;win|]) nl .>> spaces
            |> resultChoose (
                fun x ->
                    let df =
                        function
                        | TerminationSType _ -> 0
                        | TerminationLimit _ -> 1
                        | TerminationWin _ -> 2
                        | _ -> failwith "Can't touch this."
                    let t = HashSet(x,HashIdentity.FromFunctions df (fun x y -> true))
                    if t.Count = x.Length then Some t else None
                ) "duplicates detected"
            |>> SpriteCounter

        let multi_spritecounter_term =
            let spritecounter_start = skipStringCI "MultiSpriteCounter" >>. blanks
            let stype = pipe2 (skipStringCI "stype" >>. pint32 |> resultSatisfies (fun x -> x > 0) "stype's number should be >= 0") (blanks >>. skipChar '=' >>. blanks >>. word .>> blanks) (fun a b -> TerminationNumberedSType(a,b))
            let limit = skipStringCI "limit" >>. blanks >>. skipChar '=' >>. blanks >>. pint32 .>> blanks |>> (fun x -> TerminationLimit x)
            let win = pstringCI "win" >>. blanks >>. skipChar '=' >>. blanks >>. pbool .>> blanks |>> (fun x -> TerminationWin <| to_gameendtype x)
            spritecounter_start >>. manyTill (choice [|stype;limit;win|]) nl .>> spaces
            |> resultChoose (
                fun x ->
                    let df =
                        function
                        | TerminationLimit _ -> -2
                        | TerminationWin _ -> -1
                        | TerminationNumberedSType(a,b) -> a
                        | _ -> failwith "Can't touch this."
                    let t = HashSet(x,HashIdentity.FromFunctions df (fun x y -> true))
                    if t.Count = x.Length then Some t else None
                ) "duplicates detected"
            |>> MultiSpriteCounter

        choiceL [|
            timeout_term;
            spritecounter_term;
            multi_spritecounter_term
            |] "termination function"
    let terminations = many1Indents termination_choices wrong_indent

    let not_eos = notFollowedByL (pstring "EOS") "EOS can't be on the left side of the input."
    let interaction_start = pipe2 (not_eos >>. word) (blanks >>. word .>> (blanks >>. skipChar '>' >>. blanks)) (fun a b -> a,b)

    let interaction_choices = 
        choiceL [|    
            attempt (skipStringCI "stepback" >>. blanks |>> fun _ -> DelayedInteraction StepBack);
            attempt (skipStringCI "turnaround" >>. blanks |>> fun _ -> ImmediateInteraction TurnAround);
            attempt (skipStringCI "killsprite" >>. blanks |>> fun x -> ImmediateInteraction KillSprite);
            attempt (skipStringCI "collectResource" >>. blanks |>> fun x -> ImmediateInteraction <| CollectResource -1); // -1 is not used here, instead in fungen this type is used to send messages.
            attempt (
                pipe2
                    (skipStringCI "killIfFromAbove" >>. blanks)
                    (opt (skipStringCI "scoreChange" >>. blanks >>. skipChar '=' >>. blanks >>. pint32))
                    (fun _ a -> 
                        match a with
                        | Some a -> ImmediateInteraction <| KillIfFromAbove a
                        | None -> ImmediateInteraction <| KillIfFromAbove 0));
            attempt (skipStringCI "scorechange" >>. blanks >>. skipChar '=' >>. blanks >>. pint32 .>> blanks |>> fun x -> ImmediateInteraction <| ScoreChange x);
            attempt (skipStringCI "transformTo" >>. blanks >>. skipString "stype" >>. blanks >>. pchar '=' >>. blanks >>. word .>> blanks |>> fun x -> ImmediateInteraction <| TransformTo x);
            attempt 
                (pipe4 
                    (skipStringCI "killIfOtherHasMore" >>. blanks >>. skipString "resource" >>. blanks >>. pchar '=' >>. blanks) 
                    word 
                    (blanks >>. skipString "limit" >>. blanks >>. pchar '=' >>. blanks)
                    pint32 
                    (fun _ a _ b -> ImmediateInteraction <| KillIfOtherHasMore (a,b)));
            attempt (skipStringCI "clonesprite" >>. blanks |>> fun x -> ImmediateInteraction CloneSprite);
            |] "effect"
    let interaction = pipe4 interaction_start blanks (manyTill interaction_choices nl) blanks (fun a _ c _ -> Interaction(a,c))
    let interactions = many1Indents interaction wrong_indent

    let sets = 
        many1Indents (
            choice [|
                pipe4 (skipString "SpriteSet") spaces sprites spaces (fun _ _ c _ -> SpriteSet c);
                pipe4 (skipString "LevelMapping") spaces level_mappings spaces (fun _ _ c _ -> LevelMappingSet c);
                pipe4 (skipString "TerminationSet") spaces terminations spaces (fun _ _ c _ -> TerminationSet c);
                pipe4 (skipString "InteractionSet") spaces interactions spaces (fun _ _ c _-> InteractionSet c);
                    |]) wrong_indent

    let opening = (spaces >>. skipString "BasicGame" >>. spaces) >>. sets .>> eof

let RunVGDLParser (spec: string) = 
    run Inner.opening (spec.Replace("\t","    ")) // Tabs can cause parsing errors.
    
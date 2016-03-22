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
| Fleeing
| Chaser
| Portal
| Passive
| OrientedFlicker
| RandomMissile
| Spreader
| OrientedAvatar
| RandomAltChaser
| PathChaser

type PhysicsTypes =
| GridPhysics

type AttributeTypes =
| MainClass of MainClassTypes
// Attributes
| ShootTypeAttr of string
| CooldownAttr of int
| ProbabilityAttr of float
| OrientationAttr of float32 * float32
| ColorAttr of Color
| SpeedAttr of float
| ImageAttr of string
| TotalAttr of int
| SingletonAttr of bool
| ShrinkFactorAttr of float
| LimitAttr of int
| PhysicsTypeAttr of PhysicsTypes
| PortalAttr of bool
| ValueAttr of int
| SpreadProbAttr of float
| AmmoAttr of string
| RotateInPlaceAttr of bool
| EpsilonAttr of float
| ShootType1Attr of string
| ShootType2Attr of string
| InvisibleAttr of bool
| MinAmmoAttr of int
| AmmoCostAttr of int

type GameEndTypes = Continue | Win | Lose

type TerminationArguments =
| TerminationLimit of int
| TerminationWin of GameEndTypes
| TerminationSType of string
| TerminationNumberedSType of int * string

type TerminationSetTypes =
| Timeout of HashSet<TerminationArguments>
| SpriteCounter of HashSet<TerminationArguments>
| MultiSpriteCounter of HashSet<TerminationArguments>

type TerminationSetTaggedTypes =
| TimeoutTagged of int * GameEndTypes
| SpriteCounterTagged of int * int * GameEndTypes
| MultiSpriteCounterTagged of int [] * int * GameEndTypes

type SpriteSetTypes =
| Sprite of string * HashSet<AttributeTypes>
| SubSprites of SpriteSetTypes list

type InteractionArguments =
| InteractionScoreChange of int
| InteractionResource of string
| InteractionLimit of int
| InteractionStype of string
| InteractionValue of int

type InteractionTypesImmediate =
| TurnAround
| CollectResource of HashSet<InteractionArguments>
| KillIfFromAbove of HashSet<InteractionArguments>
| KillIfOtherHasMore of HashSet<InteractionArguments>
| KillSprite of HashSet<InteractionArguments>
| TransformTo of HashSet<InteractionArguments>
| CloneSprite
| ChangeResource of HashSet<InteractionArguments>
| PullWithIt
| KillIfHasLess of HashSet<InteractionArguments>
| KillIfHasMore of HashSet<InteractionArguments>
| TeleportToExit
| BounceForward
| SpawnIfHasMore of HashSet<InteractionArguments>
| SpawnIfHasLess of HashSet<InteractionArguments>

type InteractionTypesDelayed =
| StepBack
| ReverseDirection
| WrapAround
| FlipDirection

type InteractionTypesSecondary =
| StepBackSecondary

type InteractionTypesTagged =
| StepBackTagged
| StepBackAndClearPullWithItTagged
| TurnAroundTagged
| CollectResourceTagged of resource : int * value : int * scorechange : int
| KillIfFromAboveTagged of scorechange : int
| KillIfOtherHasMoreTagged of resource : int * limit : int * scorechange : int
| KillSpriteTagged of scorechange : int
| TransformToTagged of stype : int * scoreChange : int
| CloneSpriteTagged
| ChangeResourceTagged of resource : int * value : int * scorechange : int
| PullWithItTagged
| KillIfHasLessTagged of resource : int * limit : int * scorechange : int
| KillIfHasMoreTagged of resource : int * limit : int * scorechange : int
| WrapAroundTagged
| ReverseDirectionTagged
| TeleportToExitTagged
| BounceForwardTagged
| FlipDirectionTagged
| SpawnIfHasMoreTagged of resource : int * stype : int * limit : int * scorechange : int
| SpawnIfHasLessTagged of resource : int * stype : int * limit : int * scorechange : int

type InteractionTypes =
| InteractionTypesDelayed of InteractionTypesDelayed
| InteractionTypesImmediate of InteractionTypesImmediate
| InteractionTypesImmediateTagged of InteractionTypesTagged
| InteractionTypesSecondary of InteractionTypesSecondary

type InteractionSetTypes =
| Interaction of (string * string) * InteractionTypes

type SpriteParserTypes =
| SpriteSet of SpriteSetTypes list
| TerminationSet of TerminationSetTypes list
| InteractionSet of InteractionSetTypes list
| LevelMappingSet of Map<char,string list>


module ColorsConstants =
    let color_parsers = 
        let GREEN_parser = attempt (pstringCI "GREEN" |>> (fun x -> ColorAttr <| Color.Green))
        let BLUE_parser = attempt (pstringCI "BLUE" |>> (fun x -> ColorAttr <| Color.Blue))
        let RED_parser = attempt (pstringCI "RED" |>> (fun x -> ColorAttr <| Color.Red))
        let GRAY_parser = attempt (pstringCI "GRAY" |>> (fun x -> ColorAttr <| Color.Gray))
        let WHITE_parser = attempt (pstringCI "WHITE" |>> (fun x -> ColorAttr <| Color.White))
        let BROWN_parser = attempt (pstringCI "BROWN" |>> (fun x -> ColorAttr <| Color.Brown))
        let BLACK_parser = attempt (pstringCI "BLACK" |>> (fun x -> ColorAttr <| Color.Black))
        let ORANGE_parser = attempt (pstringCI "ORANGE" |>> (fun x -> ColorAttr <| Color.Orange))
        let YELLOW_parser = attempt (pstringCI "YELLOW" |>> (fun x -> ColorAttr <| Color.Yellow))
        let PINK_parser = attempt (pstringCI "PINK" |>> (fun x -> ColorAttr <| Color.Pink))
        let GOLD_parser = attempt (pstringCI "GOLD" |>> (fun x -> ColorAttr <| Color.Gold))
        let LIGHTRED_parser = attempt (pstringCI "LIGHTRED" |>> (fun x -> ColorAttr <| Color(250,50,50)))
        let LIGHTORANGE_parser = attempt (pstringCI "LIGHTORANGE" |>> (fun x -> ColorAttr <| Color(250,200,100)))
        let LIGHTBLUE_parser = attempt (pstringCI "LIGHTBLUE" |>> (fun x -> ColorAttr <| Color(50,100,250)))
        let LIGHTGREEN_parser = attempt (pstringCI "LIGHTGREEN" |>> (fun x -> ColorAttr <| Color(50,250,50)))
        let LIGHTGRAY_parser = attempt (pstringCI "LIGHTGRAY" |>> (fun x -> ColorAttr <| Color(150,150,150)))
        let DARKGRAY_parser = attempt (pstringCI "DARKGRAY" |>> (fun x -> ColorAttr <| Color(30,30,30)))
        let DARKBLUE_parser = attempt (pstringCI "DARKBLUE" |>> (fun x -> ColorAttr <| Color(20,20,100)))
        let LIGHTYELLOW_parser = attempt (pstringCI "LIGHTYELLOW" |>> (fun x -> ColorAttr <| Color.LightYellow))

        choice [|GREEN_parser;BLUE_parser;RED_parser;GRAY_parser;WHITE_parser;BROWN_parser;BLACK_parser;ORANGE_parser;YELLOW_parser;PINK_parser;GOLD_parser;LIGHTRED_parser;LIGHTORANGE_parser;LIGHTBLUE_parser;LIGHTGREEN_parser;LIGHTGRAY_parser;DARKGRAY_parser;DARKBLUE_parser;LIGHTYELLOW_parser|]

module OrientationConstants =
    let UP = (0.0f, -1.0f)
    let DOWN = (0.0f, 1.0f)
    let LEFT = (-1.0f, 0.0f)
    let RIGHT = (1.0f, 0.0f)
    let DOWN_RIGHT = (1.0f, 1.0f)

    let orientation_parsers =
        let UP_parser = attempt (pstringCI "UP" |>> (fun x -> OrientationAttr UP))
        let DOWN_parser = attempt (pstringCI "DOWN" |>> (fun x -> OrientationAttr DOWN))
        let LEFT_parser = attempt (pstringCI "LEFT" |>> (fun x -> OrientationAttr LEFT))
        let RIGHT_parser = attempt (pstringCI "RIGHT" |>> (fun x -> OrientationAttr RIGHT))

        choice [|UP_parser;DOWN_parser;LEFT_parser;RIGHT_parser|]


module Inner =
    type ExpectationEnum =
    | SAME = 0
    | SAME_UP = 1

    let rec many1Indents (same: Parser<_,_>) (up: Parser<_,_>) (stream: CharStream<_>) =
        let getIndent() =
            let mutable indentation = stream.SkipNewlineThenWhitespace(4, false)
            while stream.Peek() = '#' do
                stream.SkipRestOfLine(false) // skip comment
                indentation <- stream.SkipNewlineThenWhitespace(4, false)
            if indentation = -1 then stream.Column-1L |> int else indentation // If at newline skip it, else set indent to column.

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

    let inline resultSatisfies predicate msg (p: Parser<_,_>) : Parser<_,_> =
        let error = messageError msg
        fun stream ->
            let state = stream.State
            let reply = p stream
            if reply.Status <> Ok || predicate reply.Result then reply
            else
                stream.BacktrackTo(state) // backtrack to beginning
                Reply(Error, error)

    let inline resultChoose predicate msg (p: Parser<_,_>) : Parser<_,_> =
        fun stream ->
            let state = stream.State
            let reply = p stream
            if reply.Status = Ok then
                match predicate reply.Result with
                | Some x -> Reply(x)
                | None -> 
                    stream.BacktrackTo(state) // backtrack to beginning
                    Reply(Error, messageError msg)
            else
                Reply(reply.Status, reply.Error)

    let blanks = skipManySatisfy <| isAnyOf " "
    let identifier = many1Satisfy2L isAsciiLetter (fun x -> isAsciiLetter x || isAnyOf "_" x || isDigit x) "identifier" |>> fun x -> x.ToLower()
    let pbool = stringCIReturn "true" true <|> stringCIReturn "false" false

    // TODO: Work on this more. Replace skipnewline with followedby newline or something like that.
    let nl = (skipNewline >>. blanks) <|> eof <|> (skipChar '#' >>. skipRestOfLine false)

    let distinction_identity =
        let distinction_func = 
            function
            | MainClass _ -> 0
            // Only one main class.
            | ShootTypeAttr _ -> 1
            | CooldownAttr _ -> 2
            | ProbabilityAttr _ -> 3
            | OrientationAttr _ -> 4
            | ColorAttr _ -> 5
            | SpeedAttr _ -> 6
            | ImageAttr _ -> 7
            | TotalAttr _ -> 8
            | SingletonAttr _ -> 9
            | ShrinkFactorAttr _ -> 10
            | LimitAttr _ -> 11
            | PhysicsTypeAttr _ -> 12
            | PortalAttr _ -> 13
            | ValueAttr _ -> 14
            | SpreadProbAttr _ -> 15
            | AmmoAttr _ -> 16
            | RotateInPlaceAttr _ -> 17
            | EpsilonAttr _ -> 18
            | ShootType1Attr _ -> 19
            | ShootType2Attr _ -> 20
            | InvisibleAttr _ -> 21
            | MinAmmoAttr _ -> 22
            | AmmoCostAttr _ -> 23

        HashIdentity.FromFunctions distinction_func (fun x y -> true)


        
    let sprites = 
        let sprite_start = identifier .>> (blanks >>. skipChar '>' >>. blanks)

        let attribute = 
            [|    
            skipStringCI "Immovable" >>. blanks |>> (fun _ -> MainClass Immovable);
            skipStringCI "FlakAvatar" >>. blanks |>> (fun _ -> MainClass FlakAvatar);
            skipStringCI "Bomber" >>. blanks |>> (fun _ -> MainClass Bomber);
            skipStringCI "Spawnpoint" >>. blanks |>> (fun _ -> MainClass Spawnpoint);
            skipStringCI "Missile" >>. blanks |>> (fun _ -> MainClass Missile);
            skipStringCI "ShootAvatar" >>. blanks |>> (fun _ -> MainClass ShootAvatar);
            skipStringCI "RandomNPC" >>. blanks |>> (fun _ -> MainClass RandomNPC);
            skipStringCI "Flicker" >>. blanks |>> (fun _ -> MainClass Flicker);
            skipStringCI "Resource" >>. blanks |>> (fun _ -> MainClass Resource);
            skipStringCI "Door" >>. blanks |>> (fun _ -> MainClass Door);
            skipStringCI "MovingAvatar" >>. blanks |>> (fun _ -> MainClass MovingAvatar);
            skipStringCI "Fleeing" >>. blanks |>> (fun _ -> MainClass Fleeing);
            skipStringCI "Chaser" >>. blanks |>> (fun _ -> MainClass Chaser);
            skipStringCI "Passive" >>. blanks |>> (fun _ -> MainClass Passive);
            skipStringCI "OrientedFlicker" >>. blanks |>> (fun _ -> MainClass OrientedFlicker);
            skipStringCI "RandomMissile" >>. blanks |>> (fun _ -> MainClass RandomMissile);
            skipStringCI "Spreader" >>. blanks |>> (fun _ -> MainClass Spreader);
            skipStringCI "OrientedAvatar" >>. blanks |>> (fun _ -> MainClass OrientedAvatar);
            skipStringCI "RandomAltChaser" >>. blanks |>> (fun _ -> MainClass RandomAltChaser);
            skipStringCI "PathChaser" >>. blanks |>> (fun _ -> MainClass PathChaser);

            skipStringCI "Portal" >>. blanks >>. (opt (skipChar '=' >>. blanks >>. pbool .>> blanks)) // portal can be a class or an attribute depending on if it is followed by '='
                |>> (function | None -> MainClass Portal | Some v -> PortalAttr v);

            skipStringCI "cooldown" >>. blanks >>. skipChar '=' >>. blanks >>. pint32 .>> blanks |>> (fun x -> CooldownAttr x);
            skipStringCI "prob" >>. blanks >>. skipChar '=' >>. blanks >>. pfloat .>> blanks |>> (fun x -> ProbabilityAttr x);
            skipStringCI "orientation" >>. blanks >>. skipChar '=' >>. blanks >>. OrientationConstants.orientation_parsers .>> blanks ;
            skipStringCI "color" >>. blanks >>. skipChar '=' >>. blanks >>. ColorsConstants.color_parsers .>> blanks ;
            skipStringCI "speed" >>. blanks >>. skipChar '=' >>. blanks >>. pfloat .>> blanks |>> (fun x -> SpeedAttr x);
            skipStringCI "img" >>. blanks >>. skipChar '=' >>. blanks >>. (identifier .>> optional (pstring ".png")) .>> blanks |>> (fun x -> ImageAttr x);
            skipStringCI "total" >>. blanks >>. skipChar '=' >>. blanks >>. pint32 .>> blanks |>> (fun x -> TotalAttr x);
            skipStringCI "singleton" >>. blanks >>. skipChar '=' >>. blanks >>. pbool .>> blanks |>> (fun x -> SingletonAttr x);
            skipStringCI "shrinkfactor" >>. blanks >>. skipChar '=' >>. blanks >>. pfloat .>> blanks |>> (fun x -> ShrinkFactorAttr x);
            skipStringCI "limit" >>. blanks >>. skipChar '=' >>. blanks >>. pint32 .>> blanks |>> (fun x -> LimitAttr x);
            skipStringCI "physicstype" >>. blanks >>. skipChar '=' >>. blanks >>. 
                (choice 
                    [|skipStringCI "GridPhysics" .>> blanks |>> (fun x -> PhysicsTypeAttr GridPhysics)|]);
            skipStringCI "value" >>. blanks >>. skipChar '=' >>. blanks >>. pint32 .>> blanks |>> (fun x -> ValueAttr x);
            skipStringCI "spreadprob" >>. blanks >>. skipChar '=' >>. blanks >>. pfloat .>> blanks |>> (fun x -> SpreadProbAttr x);
            skipStringCI "ammo" >>. blanks >>. skipChar '=' >>. blanks >>. identifier .>> blanks |>> (fun x -> AmmoAttr x);
            skipStringCI "rotateInPlace" >>. blanks >>. skipChar '=' >>. blanks >>. pbool .>> blanks |>> (fun x -> RotateInPlaceAttr x);
            skipStringCI "epsilon" >>. blanks >>. skipChar '=' >>. blanks >>. pfloat .>> blanks |>> (fun x -> EpsilonAttr x);
            skipStringCI "stype" >>. blanks >>. skipChar '=' >>. blanks >>. identifier .>> blanks |>> (fun x -> ShootTypeAttr x);
            skipStringCI "stype1" >>. blanks >>. skipChar '=' >>. blanks >>. identifier .>> blanks |>> (fun x -> ShootType1Attr x);
            skipStringCI "stype2" >>. blanks >>. skipChar '=' >>. blanks >>. identifier .>> blanks |>> (fun x -> ShootType2Attr x);
            skipStringCI "invisible" >>. blanks >>. skipChar '=' >>. blanks >>. pbool .>> blanks |>> (fun x -> InvisibleAttr x);
            skipStringCI "minAmmo" >>. blanks >>. skipChar '=' >>. blanks >>. pint32 .>> blanks |>> (fun x -> MinAmmoAttr x);
            skipStringCI "ammoCost" >>. blanks >>. skipChar '=' >>. blanks >>. pint32 .>> blanks |>> (fun x -> AmmoCostAttr x);
            |] 
            |> Array.map attempt
            |> fun ar -> choiceL ar "attribute"

        let sprite =
            let manyAttrs =
                manyTill attribute nl
                |> resultChoose (
                    fun x ->
                        let t = HashSet(x,distinction_identity)
                        if t.Count = x.Length then Some t else None
                    ) "duplicate detected."

            pipe4 sprite_start blanks manyAttrs blanks (fun a b c _ -> Sprite(a,c))
        
        let rec y f x = f (y f) x // The Y Combinator
        //let rec sprites_up = many1Indents sprite sprites_up |>> (fun x -> SubSprites x) Does not work
        let sprites_up = y (fun f -> many1Indents sprite f |>> (fun x -> SubSprites x))
        many1Indents sprite sprites_up

    let wrong_indent = (fun _ -> Reply(Error, messageError "child indents not aligned")) 
    
    let level_mappings = 
        let level_mapping = pipe3 anyChar (blanks >>. (skipChar '>') >>. blanks) (manyTill (identifier .>> blanks) nl) (fun a b c -> a, c)

        many1Indents level_mapping wrong_indent 
        |> resultChoose
            (fun x ->
            let t = Map(x)
            if t.Count = x.Length then Some t else None) "duplicate entries in LevelMapping"
        |>> fun x -> if x.ContainsKey('w') = false then x.Add('w',["wall"]) else x
        |>> fun x -> if x.ContainsKey('A') = false then x.Add('A',["avatar"]) else x

    let to_gameendtype =
        function
        | true -> Win
        | false -> Lose

    let terminations = 
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
                let stype = skipStringCI "stype" >>. blanks >>. skipChar '=' >>. blanks >>. identifier .>> blanks |>> (fun x -> TerminationSType x)
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
                let stype = pipe2 (skipStringCI "stype" >>. pint32 |> resultSatisfies (fun x -> x > 0 && x < 100) "stype's number should be x > 0 && x < 100") (blanks >>. skipChar '=' >>. blanks >>. identifier .>> blanks) (fun a b -> TerminationNumberedSType(a,b))
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
    
        many1Indents termination_choices wrong_indent

    let interactions = 
        let interaction_choices = 
            let dist =
                HashIdentity.FromFunctions (
                    function
                    | InteractionScoreChange _ -> 0
                    | InteractionResource _ -> 1
                    | InteractionLimit _ -> 2
                    | InteractionValue _ -> 3
                    | InteractionStype _ -> 4
                    ) (fun _ _ -> true)

            let scoreChange = skipStringCI "scoreChange" >>. blanks >>. skipChar '=' >>. blanks >>. pint32 .>> blanks |>> InteractionScoreChange; 
            let resource = skipStringCI "resource" >>. blanks >>. skipChar '=' >>. blanks >>. identifier .>> blanks |>> InteractionResource; 
            let limit = skipStringCI "limit" >>. blanks >>. skipChar '=' >>. blanks >>. pint32 .>> blanks |>> InteractionLimit; 
            let value = skipStringCI "value" >>. blanks >>. skipChar '=' >>. blanks >>. pint32 .>> blanks |>> InteractionValue;
            let stype = skipStringCI "stype" >>. blanks >>. skipChar '=' >>. blanks >>. identifier .>> blanks |>> InteractionStype;
            let interaction_arguments choices =
                choices
                |> Array.map attempt
                |> fun ar -> choiceL ar "interaction argument"
                |> fun x -> manyTill x nl
                |> resultChoose (
                    fun x ->
                    let h = HashSet(x,dist)
                    if h.Count = x.Length then Some h else None
                    ) "duplicates detected"

            [|    
            skipStringCI "stepback" >>. blanks |>> fun _ -> InteractionTypesDelayed StepBack;
            skipStringCI "turnaround" >>. blanks |>> fun _ -> InteractionTypesImmediate TurnAround;
            skipStringCI "killsprite" >>. blanks >>. (interaction_arguments [|scoreChange|]) |>> fun x -> InteractionTypesImmediate <| KillSprite x;
            skipStringCI "collectResource" >>. blanks >>. (interaction_arguments [|scoreChange|]) |>> fun x -> InteractionTypesImmediate <| CollectResource x;
            skipStringCI "killIfFromAbove" >>. blanks >>. (interaction_arguments [|scoreChange|]) |>> fun x -> InteractionTypesImmediate <| KillIfFromAbove x;
            skipStringCI "transformTo" >>. blanks >>. (interaction_arguments [|stype; scoreChange|]) |>> fun x -> InteractionTypesImmediate <| TransformTo x;
            skipStringCI "killIfOtherHasMore" >>. blanks >>. (interaction_arguments [|resource; limit; scoreChange|]) |>> fun x -> InteractionTypesImmediate <| KillIfOtherHasMore x;
            skipStringCI "clonesprite" >>. blanks |>> fun x -> InteractionTypesImmediate <| CloneSprite;
            skipStringCI "pullWithIt" >>. blanks |>> fun x -> InteractionTypesImmediate <| PullWithIt;
            skipStringCI "wrapAround" >>. blanks |>> fun x -> InteractionTypesDelayed <| WrapAround;
            skipStringCI "changeResource" >>. blanks >>. (interaction_arguments [|resource; value; scoreChange|]) |>> fun x -> InteractionTypesImmediate <| ChangeResource x;
            skipStringCI "killIfHasLess" >>. blanks >>. (interaction_arguments [|resource; limit; scoreChange|]) |>> fun x -> InteractionTypesImmediate <| KillIfHasLess x;
            skipStringCI "killIfHasMore" >>. blanks >>. (interaction_arguments [|resource; limit; scoreChange|]) |>> fun x -> InteractionTypesImmediate <| KillIfHasMore x;
            skipStringCI "reverseDirection" >>. blanks |>> fun x -> InteractionTypesDelayed <| ReverseDirection;
            skipStringCI "teleportToExit" >>. blanks |>> fun x -> InteractionTypesImmediate <| TeleportToExit;
            skipStringCI "bounceForward" >>. blanks |>> fun x -> InteractionTypesImmediate <| BounceForward;
            skipStringCI "undoall" >>. blanks |>> fun x -> InteractionTypesSecondary <| StepBackSecondary;
            skipStringCI "flipDirection" >>. blanks |>> fun x -> InteractionTypesDelayed <| FlipDirection;
            skipStringCI "spawnIfHasMore" >>. blanks >>. (interaction_arguments [|resource; stype; limit; scoreChange|]) |>> fun x -> InteractionTypesImmediate <| SpawnIfHasMore x;
            skipStringCI "spawnIfHasLess" >>. blanks >>. (interaction_arguments [|resource; stype; limit; scoreChange|]) |>> fun x -> InteractionTypesImmediate <| SpawnIfHasLess x;
            |] 
            |> Array.map attempt
            |> fun ar -> choiceL ar "effect" 

        let not_eos = notFollowedByL (pstring "EOS") "EOS can't be on the left side of the input."
        let interaction_start = pipe2 (not_eos >>. identifier) (blanks >>. identifier .>> (blanks >>. skipChar '>' >>. blanks)) (fun a b -> a,b)

        let interaction = pipe4 interaction_start blanks interaction_choices blanks (fun a _ c _ -> Interaction(a,c))        
        
        many1Indents interaction wrong_indent |>> fun x -> x |> List.rev

    let sets = 
        many1Indents (
            choice [|
                pipe4 (skipString "SpriteSet") spaces sprites spaces (fun _ _ c _ -> SpriteSet c);
                pipe4 (skipString "LevelMapping") spaces level_mappings spaces (fun _ _ c _ -> LevelMappingSet c);
                pipe4 (skipString "TerminationSet") spaces terminations spaces (fun _ _ c _ -> TerminationSet c);
                pipe4 (skipString "InteractionSet") spaces interactions spaces (fun _ _ c _-> InteractionSet c);
                    |]) wrong_indent

    let opening = 
        let opening_args =
            skipStringCI "square_size" >>. blanks >>. skipChar '=' >>. blanks >>. pint32 >>. spaces // square_size does nothing for now
        (spaces >>. skipString "BasicGame" >>. blanks >>. optional opening_args) >>. sets .>> eof

let RunVGDLParser (spec: string) = 
    run Inner.opening (spec.Replace("\t","    ")) // Tabs can cause parsing errors.
    
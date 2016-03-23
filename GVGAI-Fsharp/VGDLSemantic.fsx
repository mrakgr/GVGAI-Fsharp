module VGDLSemantic
// Various tree transformations and error checking to make the raw tree from the parsing step more useful.
// The semantic analysis stage.

#if INTERACTIVE
#load "VGDLParser.fsx"
#endif

open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open VGDLParser
open FParsec
open System.Collections.Generic
open System.Linq

let SPRITE_WIDTH, SPRITE_HEIGHT = 20,20
let STANDARD_SPEED = 3.0f
let TICK_ADJUSTER = 6

type AttributeTypesRecord =
    {
    mclass : MainClassTypes
    shootType : int
    cooldown : int
    probability : float
    orientation : float32 * float32
    color : Color
    speed : float32
    image : string
    total : int
    singleton : bool
    shrinkfactor : float
    limit : int
    value : int
    spreadprob : float
    ammo : int
    rotate_in_place : bool
    epsilon : float
    shootType1 : int
    shootType2 : int
    invisible : bool
    min_ammo : int
    ammo_cost : int
    }

type VGDLSprite = 
    struct
    val texture : Texture2D option
    val position : Vector2
    val orientation : Vector2
    val speed : float32
    val id : int
    val elapsed_time : int
    val total : int
    val resources : Map<int, int> // An immutable map.
    val mclass : MainClassTypes
    val shootType : int 
    val cooldown : int
    val probability : float
    val limit : int
    val duration : int
    val ammo : int
    val min_ammo : int
    val ammo_cost : int
        
    new(texture,position,orientation,speed,id,elapsed_time,total,resources,mclass,shootType,cooldown,probability,limit,duration,ammo,min_ammo,ammo_cost) =
        {
        texture=texture;position=position;orientation=orientation;speed=speed;
        id=id;elapsed_time=elapsed_time;total=total;resources=resources;
        mclass=mclass;shootType=shootType;cooldown=cooldown;probability=probability;limit=limit;
        duration=duration;ammo=ammo;min_ammo=min_ammo;ammo_cost=ammo_cost;
        }
    end

    /// The default init.
    static member def= // Should be synced with the default record, but no matter.
        new VGDLSprite(None,
            position=Vector2(0.0f,0.0f),
            orientation=Vector2(0.0f,0.0f),
            speed=0.0f,
            id= -1,
            elapsed_time=0,
            total=0,
            resources=Map.empty,
            mclass=NoClass,
            shootType= -1,
            cooldown=0,
            probability=0.0,
            limit=0,
            duration=0,
            ammo= -1,
            min_ammo=1,
            ammo_cost=1
            )

// The type that plays the role of a transitioner.
type VGDLMutableSprite =
    struct
    val mutable texture : Texture2D option
    val mutable position : Vector2
    val mutable orientation : Vector2
    val mutable speed : float32
    val mutable id : int
    val mutable elapsed_time : int
    val mutable total : int
    val mutable resources : Map<int, int>
    val mutable mclass : MainClassTypes
    val mutable shootType : int
    val mutable cooldown : int
    val mutable probability : float
    val mutable limit : int
    val mutable duration : int
    val mutable ammo : int
    val mutable min_ammo : int
    val mutable ammo_cost : int
        
    new(texture,position,orientation,speed,id,elapsed_time,total,resources,mclass,shootType,cooldown,probability,limit,duration,ammo,min_ammo,ammo_cost) =
        {
        texture=texture;position=position;orientation=orientation;speed=speed;
        id=id;elapsed_time=elapsed_time;total=total;resources=resources;
        mclass=mclass;shootType=shootType;cooldown=cooldown;probability=probability;limit=limit;
        duration=duration;ammo=ammo;min_ammo=min_ammo;ammo_cost=ammo_cost;
        }
    end

    static member inline fromIm (x: VGDLSprite) =
        new VGDLMutableSprite(x.texture,x.position,x.orientation,x.speed,x.id,x.elapsed_time,x.total,x.resources,x.mclass,x.shootType,x.cooldown,x.probability,x.limit,x.duration,x.ammo,x.min_ammo,x.ammo_cost)

    member inline x.toIm =
        new VGDLSprite(x.texture,x.position,x.orientation,x.speed,x.id,x.elapsed_time,x.total,x.resources,x.mclass,x.shootType,x.cooldown,x.probability,x.limit,x.duration,x.ammo,x.min_ammo,x.ammo_cost)

// Double buffered state type.
type StateType =
    struct
    val mutable sprite1 : VGDLSprite
    val mutable sprite2 : VGDLSprite
    val mutable sprite3 : VGDLSprite
    val mutable sprite4 : VGDLSprite
    new (a1,a2,a3,a4) = {sprite1=a1;sprite2=a2;sprite3=a3;sprite4=a4}
    new (sprite1) = {sprite1=sprite1;sprite2=VGDLSprite.def;sprite3=VGDLSprite.def;sprite4=VGDLSprite.def}
    end

    static member def = StateType(VGDLSprite.def,VGDLSprite.def,VGDLSprite.def,VGDLSprite.def)

type private MainRec = 
    {
    sprite_set : SpriteSetTypes list
    termination_set : TerminationSetTypes list 
    interaction_set : InteractionSetTypes list
    level_mapping_set : Map<char,string list>
    }

type private TagRecord =
    {
    scoreChange : int
    resource : int
    limit : int
    stype : int
    value : int
    }

let private default_tagrec =
    {
    scoreChange = 0
    resource = -1
    limit = 0
    stype = -1
    value = 1
    }

let DOWN = 
    let t = OrientationConstants.DOWN 
    Vector2(fst t, snd t)
let UP = 
    let t = OrientationConstants.UP 
    Vector2(fst t, snd t)
let LEFT = 
    let t = OrientationConstants.LEFT
    Vector2(fst t, snd t)
let RIGHT = 
    let t = OrientationConstants.RIGHT 
    Vector2(fst t, snd t)

let ORIENTATIONS = [|DOWN;UP;LEFT;RIGHT|]
let rng = System.Random()

let runSematic gameDesc =
    let tree_rec = 
        match RunVGDLParser gameDesc with
        | Success(x,_,_) -> x
        | Failure(x,_,_) -> failwithf "parsing failed\n%s" x
        |> fun tree -> // I am using functional composition as a style here. 
                        // The purpose of this is to keep the number of free variables down to a minimum.
        {
        sprite_set = 
            tree |> List.filter (fun x -> match x with | SpriteSet _ -> true | _ -> false) 
                    |> fun x -> if x.Length > 1 then failwith "Duplicate SpriteSets not allowed!"
                                else if x.Length < 1 then failwith "No SpriteSet!"
                                else x.Head
                    |> function SpriteSet x -> x | _ -> failwith "Won't trigger."
        termination_set =
            tree |> List.filter (fun x -> match x with | TerminationSet _ -> true | _ -> false) 
                    |> fun x -> if x.Length > 1 then failwith "Duplicate TerminationSets not allowed!"
                                else if x.Length < 1 then failwith "No TerminationSet!"
                                else x.Head
                    |> function TerminationSet x -> x | _ -> failwith "Won't trigger."
        interaction_set =
            tree |> List.filter (fun x -> match x with | InteractionSet _ -> true | _ -> false) 
                    |> fun x -> if x.Length > 1 then failwith "Duplicate InteractionSets not allowed!"
                                else if x.Length < 1 then failwith "No InteractionSet!"
                                else x.Head
                    |> function InteractionSet x -> x | _ -> failwith "Won't trigger."
        level_mapping_set =
            tree |> List.filter (fun x -> match x with | LevelMappingSet _ -> true | _ -> false) 
                    |> fun x -> if x.Length > 1 then failwith "Duplicate LevelMappingSets not allowed!"
                                else if x.Length < 1 then failwith "No LevelMappingSet!"
                                else x.Head
                    |> function LevelMappingSet x -> x | _ -> failwith "Won't trigger."
        }

    let id_map, tree_map, reverse_id_map =
        /// Flattens the tree and propagates the parent attributes to the children unless they are overriden.
        let rec flattenSpriteSet (spriteset: _ list) =
            let map_add_if (m : Map<_,_>) (k, v) =
                if m.ContainsKey k then failwithf "In SpriteSet duplicate names are forbidden: %s" k
                else m.Add(k,v)
            let get_sprite_attrs s =
                match s with
                | Sprite(n,s) -> n,s 
                | _ -> failwith "Sprite only function."

            let rec loop map parent_attrs (tree: _ list) =
                match tree with
                | h::hy::hs ->
                    let name,attrs_child = get_sprite_attrs h
                    attrs_child.UnionWith(parent_attrs)
                    let new_map = map_add_if map (name,attrs_child)
                    match hy with
                    | SubSprites x -> 
                        let new_map' = loop new_map attrs_child x
                        loop new_map' parent_attrs hs
                    | _ -> loop new_map parent_attrs (hy::hs)
                | h::hs -> 
                    let name,attrs_child = get_sprite_attrs h
                    attrs_child.UnionWith(parent_attrs)
                    map_add_if map (name,attrs_child)
                | [] -> map
            loop Map.empty (HashSet()) spriteset

        let rec mapHierarchy (spriteset: _ list) =
            let get_sprite_name s =
                match s with
                | Sprite(n,s) -> n
                | _ -> failwith "Sprite only function."
            
            let rec loop (map: Map<_,_ list>) accum (tree: _ list) =
                match tree with
                | h::hy::hs ->
                    let parent_name = get_sprite_name h
            
                    match hy with
                    | SubSprites x -> 
                        let child_names, map = loop map [] x
                        loop (map |> Map.add parent_name child_names) (List.append (child_names) (parent_name::accum)) hs
                    | Sprite _ -> 
                        loop (map.Add(parent_name,[])) (parent_name::accum) (hy::hs)
                | h::hs -> 
                    let parent_name = get_sprite_name h
                    loop (map.Add(parent_name,[])) (parent_name::accum) hs
                | [] -> accum, map
            loop Map.empty [] spriteset
            |> fun (names, hierarchy) ->
                names,
                if hierarchy |> Map.containsKey("avatar") = false then
                    hierarchy |> Map.add "avatar" []
                else hierarchy

        let default_record =
            { 
            mclass = NoClass
            shootType = -1
            cooldown = 0
            probability = 1.0
            orientation = 0.0f,0.0f
            color = Color.Gray
            speed = STANDARD_SPEED
            image = ""
            total = Int32.MaxValue
            singleton = false
            shrinkfactor = 1.0
            limit = Int32.MaxValue
            value = 1
            spreadprob = 1.0
            ammo = -1
            rotate_in_place = false
            epsilon = 0.0
            shootType1 = -1
            shootType2 = -1
            invisible = false
            min_ammo = 1
            ammo_cost = 1
            }

        let sprite_names, hierarchy_map = mapHierarchy tree_rec.sprite_set
        
        let id_map, reverse_id_map = 
            sprite_names 
            |> List.toArray
            |> fun x -> Array.append x [|"wall";"avatar"|]
            |> Array.distinct 
            |> Array.rev
            |> Array.mapi (fun i x -> x,i)
            |> fun x -> x.ToDictionary((fun (k,v) -> k),(fun (k,v) -> v), HashIdentity.Structural)
            |> fun x -> x.Add("null",-1); x
            |> fun x ->
                x, x.ToArray() |> fun x -> x.ToDictionary((fun kv -> kv.Value),(fun kv -> kv.Key), HashIdentity.Structural)

        let flat_spriteset = 
            flattenSpriteSet tree_rec.sprite_set
            |> fun x ->
                if x |> Map.containsKey("avatar") = false then 
                    x |> Map.add "avatar" (HashSet([|MainClass MovingAvatar|],VGDLParser.Inner.distinction_identity)) 
                else x
            |> Map.map (fun k v ->
                    Seq.fold (
                        fun state v -> 
                            match v with
                            | MainClass (ShootAvatar | FlakAvatar | MovingAvatar | OrientedAvatar | InertialAvatar as x) -> 
                                if state.image = "" then
                                    {state with mclass = x; image="avatar"}
                                else
                                    {state with mclass = x}
                            | MainClass x -> {state with mclass = x}
                            | ShootTypeAttr x -> {state with shootType = id_map.[x]}
                            | CooldownAttr x -> {state with cooldown = x*TICK_ADJUSTER}
                            | ProbabilityAttr x -> {state with probability = 1.0 - (1.0-x) ** (1.0 / (float TICK_ADJUSTER))} // Just a little thing to make the probabilities more inline with the original GVGAI library.
                            | OrientationAttr (x,y) -> {state with orientation = x,y}
                            | ColorAttr x -> {state with color = x}
                            | SpeedAttr x -> {state with speed = (STANDARD_SPEED * float32 x)}
                            | ImageAttr x -> {state with image = x}
                            | TotalAttr x -> {state with total = x}
                            | SingletonAttr x -> {state with singleton = x}
                            | ShrinkFactorAttr x -> {state with shrinkfactor = x}
                            | LimitAttr x -> {state with limit = x}
                            | PhysicsTypeAttr x -> state // TODO: This will do nothing for now.
                            | PortalAttr x -> state // This does nothing as well currently.
                            | ValueAttr x -> {state with value = x}
                            | SpreadProbAttr x -> {state with spreadprob = x}
                            | AmmoAttr x -> {state with ammo = id_map.[x]}
                            | RotateInPlaceAttr x -> {state with rotate_in_place = x}
                            | EpsilonAttr x -> {state with epsilon = 1.0 - (1.0-x) ** (1.0 / (float TICK_ADJUSTER))}
                            | ShootType1Attr x -> {state with shootType1 = id_map.[x]}
                            | ShootType2Attr x -> {state with shootType2 = id_map.[x]}
                            | InvisibleAttr x -> {state with invisible = x}
                            | MinAmmoAttr x -> {state with min_ammo = x}
                            | AmmoCostAttr x -> {state with ammo_cost = x}
                            ) default_record v
                )
            |> Map.toArray |> Array.map (fun (k,v) -> id_map.[k],v) |> Map.ofArray
            |> fun map ->
                map |> Map.map (fun k v -> 
                    match v.mclass with
                    | Spawnpoint -> 
                        if v.shootType <> -1 && v.cooldown = 0 then {v with cooldown = map.[v.shootType].cooldown} else v
                    | _ -> v
                    )

        let tree_map = 
            let t1 = flat_spriteset |> Map.toArray
            let t2 = hierarchy_map |> Map.toArray |> Array.map (fun (k,v) -> id_map.[k],v |> List.map (fun x -> id_map.[x])) |> Array.sortBy (fun (k,v) -> k)
            let id_wall = id_map.["wall"]
            let id_avatar = id_map.["avatar"]
            Array.map2 (fun (k1,v1) (k2,v2) -> if k1 = k2 then k1,(v1,v2) else failwith "Maps are not ordered") t1 t2
            |> Map.ofArray
            |> fun x -> if x.ContainsKey(id_wall) = false then x.Add(id_wall,({default_record with image="wall"},[])) else x
            |> fun x -> if x.ContainsKey(id_avatar) = false then x.Add(id_avatar,(default_record,[])) else x
            
        id_map, tree_map, reverse_id_map

    let id_hierarchy_map = 
        tree_map |> Map.map ( fun  parent (_,children) -> parent::children |> List.toArray)

    let eos, neos =
        let rec loop (eos: _ list) (neos: _ list)=
            function
            | x::xs ->
                match x with
                | Interaction((_,"eos"),_) -> loop (x::eos) neos xs
                | Interaction _ -> loop eos (x::neos) xs
            | [] -> eos,neos
        loop [] [] tree_rec.interaction_set

    let pull_with_it_exists = // If the PullWithIt interaction exists then StepBack gets converted to StepBackAndClearPullWithItTagged instead of StepBackTagged
        let rec loop =
            function
            | (Interaction(_,InteractionTypesImmediate PullWithIt))::xs -> true // Pattern matching is great.
            | x::xs -> loop xs
            | [] -> false
        loop tree_rec.interaction_set

    let interaction_tagger l r =
        let tag_hashset hashset =
            let fold_state state =
                function
                | InteractionScoreChange x -> {state with scoreChange = x}
                | InteractionResource x -> 
                    match id_map.TryGetValue x with
                    | true, v -> {state with resource = v}
                    | false, _ -> failwithf "Resource %s not found!" x
                | InteractionLimit x -> {state with limit = x}
                | InteractionStype x -> 
                    match id_map.TryGetValue x with
                    | true, v -> {state with stype = v}
                    | false, _ -> failwithf "Stype %s not found!" x
                | InteractionValue x -> {state with value = x}
            Seq.fold fold_state default_tagrec hashset
               
        function
        | TurnAround -> TurnAroundTagged
        | CollectResource x -> 
            let t = tag_hashset x
            let v = tree_map.[r] |> fst |> (fun x -> x.value) 
            CollectResourceTagged(r,v,t.scoreChange)
        | KillIfFromAbove x -> let t = tag_hashset x in KillIfFromAboveTagged t.scoreChange
        | KillIfOtherHasMore x -> let t = tag_hashset x in KillIfOtherHasMoreTagged(t.resource,t.limit,t.scoreChange)
        | KillSprite x -> let t = tag_hashset x in KillSpriteTagged t.scoreChange
        | TransformTo x -> let t = tag_hashset x in TransformToTagged(t.stype,t.scoreChange)
        | CloneSprite -> CloneSpriteTagged
        | ChangeResource x -> 
            let t = tag_hashset x 
            let v = tree_map.[t.resource] |> fst |> (fun x -> x.value) 
            ChangeResourceTagged(t.resource,(if t.value = 1 then v else t.value), t.scoreChange)
        | PullWithIt -> PullWithItTagged
        | KillIfHasLess x -> let t = tag_hashset x in KillIfHasLessTagged(t.resource,t.limit,t.scoreChange)
        | KillIfHasMore x -> let t = tag_hashset x in KillIfHasMoreTagged(t.resource,t.limit,t.scoreChange)
        | TeleportToExit x -> let t = tag_hashset x in TeleportToExitTagged(t.scoreChange)
        | BounceForward -> BounceForwardTagged
        | SpawnIfHasLess x -> let t = tag_hashset x in SpawnIfHasLessTagged(t.resource,t.stype,t.limit,t.scoreChange)
        | SpawnIfHasMore x -> let t = tag_hashset x in SpawnIfHasMoreTagged(t.resource,t.stype,t.limit,t.scoreChange)
        | AttractGaze -> AttractGazeTagged

    let interaction_tagger_del l r =
        function
        | StepBack -> if pull_with_it_exists then StepBackAndClearPullWithItTagged else StepBackTagged
        | WrapAround -> WrapAroundTagged
        | ReverseDirection -> ReverseDirectionTagged       
        | FlipDirection -> FlipDirectionTagged    

    let tagged_eos = 
        let buf_im = Array.init tree_map.Count (fun _ -> ResizeArray())
        let buf_del = Array.init tree_map.Count (fun _ -> ResizeArray())
        let buf_sec = Array.init tree_map.Count (fun _ -> ResizeArray())
        eos |> List.iteri 
            (fun i (Interaction((l,r),attrs)) ->
            let i=i*10
            for x in id_hierarchy_map.[id_map.[l]] do
                match attrs with
                | InteractionTypesImmediate attrs -> buf_im.[x].Add(interaction_tagger x -1 attrs, i)
                | InteractionTypesDelayed attrs -> buf_del.[x].Add(interaction_tagger_del x -1 attrs, i)
                | InteractionTypesSecondary attrs -> buf_sec.[x].Add(attrs)
                | _ -> failwith "Can't match this.")
            
        buf_im |> Array.map (fun x -> x.ToArray()), // Plain arrays are faster than ResizeArrays for iterating over them.
        buf_del |> Array.map (fun x -> x.ToArray()),
        buf_sec |> Array.map (fun x -> x.ToArray())

    let tagged_neos = 
        let buf_im = Array2D.init tree_map.Count tree_map.Count (fun _ _ -> ResizeArray())
        let buf_del = Array2D.init tree_map.Count tree_map.Count (fun _ _ -> ResizeArray())
        let buf_sec = Array2D.init tree_map.Count tree_map.Count (fun _ _ -> ResizeArray())
        neos |> List.iteri 
            (fun i (Interaction((l,r),attrs)) ->
                let i=i*10
                for x in id_hierarchy_map.[id_map.[l]] do
                    for y in id_hierarchy_map.[id_map.[r]] do
                        match attrs with
                        | InteractionTypesImmediate attrs -> 
                            match attrs with
                            | CollectResource _ -> buf_im.[y,x].Add(interaction_tagger y x attrs, i) // Collect resource is a special case that work on the right argument. Here I am reversing it.
                            | BounceForward -> buf_im.[x,y].Add(interaction_tagger x y attrs, i); buf_im.[y,x].Add(StepBackTagged, i+1)
                            | _ -> buf_im.[x,y].Add(interaction_tagger x y attrs, i)
                        | InteractionTypesDelayed attrs -> 
                            match attrs with
                            | StepBack -> buf_im.[x,y].Add(interaction_tagger_del x y attrs, i); buf_sec.[x,y].Add(StepBackSecondary)
                            | _ -> buf_del.[x,y].Add(interaction_tagger_del x y attrs, i)
                        | InteractionTypesSecondary attrs -> buf_sec.[x,y].Add(attrs)
                        | _ -> failwith "Can't match this.")

        buf_im |> Array2D.map (fun x -> x.ToArray()), // Plain arrays are faster than ResizeArrays for iterating over them.
        buf_del |> Array2D.map (fun x -> x.ToArray()),
        buf_sec |> Array2D.map (fun x -> x.ToArray())

    let stepbacks_neos = // For pathfinding.
        let buf_im = Array2D.zeroCreate tree_map.Count tree_map.Count
        neos |> List.iteri 
            (fun i (Interaction((l,r),attrs)) ->
                for x in id_hierarchy_map.[id_map.[l]] do
                    for y in id_hierarchy_map.[id_map.[r]] do
                        match attrs with
                        | InteractionTypesImmediate attrs -> ()
                        | InteractionTypesDelayed attrs -> 
                            match attrs with
                            | StepBack -> buf_im.[x,y] <- true; printfn "buf_im.[%A,%A] <- true" reverse_id_map.[x] reverse_id_map.[y]
                            | _ -> ()
                        | InteractionTypesSecondary attrs -> ()
                        | _ -> failwith "Can't match this.")

        buf_im
        

    let reverse_hierarchy = // Computes the reverse hierarchy from children to parents. Needed for TerminationSet counters.
        let d = Dictionary<int,HashSet<int>>(HashIdentity.Structural)
        for x in tree_map do
            let parent,children = 
                (x.Key,x.Value |> snd)
                |> fun (k,v) ->
                    k,
                    v |> List.toArray

            match d.TryGetValue parent with
            | true, dh -> ()
            | false, _ -> d.Add(parent, HashSet([|parent|],HashIdentity.Structural))

            for self in children do
                match d.TryGetValue self with
                | true, dh -> dh.Add parent |> ignore
                | false, _ -> d.Add(self, HashSet([|self;parent|],HashIdentity.Structural))
        d.ToArray() |> Array.sortBy (fun x -> x.Key) |> Array.map (fun x -> x.Value |> Seq.toArray |> Array.sort)
        
    let initializer_map (texture_dict : Dictionary<string, Texture2D>) (game : Game) =
        /// Get the image name of a sprite otherwise it returns ""
        let image_name_of sprite_name = 
            tree_map.[sprite_name] 
            |> fst 
            |> fun x -> x.image
        [|
        for p in tree_map do
            let sprite_id,r = p.Key, p.Value |> fst
            let texture = 
                if r.invisible = false then
                    match texture_dict.TryGetValue (image_name_of sprite_id) with
                    | true, v -> v |> Some
                    | false, _ -> 
                        let t = new Texture2D(game.GraphicsDevice, SPRITE_WIDTH, SPRITE_HEIGHT)
                        t.SetData(Array.init (SPRITE_WIDTH*SPRITE_HEIGHT) (fun _ -> r.color))
                        t |> Some
                else None
            let init position direction = 
                let velocity = 
                    match r.mclass with
                    | FlakAvatar -> Vector2(r.speed,0.0f)
                    | RandomMissile -> ORIENTATIONS.[rng.Next(0,ORIENTATIONS.Length)]*r.speed
                    | Bomber -> Vector2(r.speed,0.0f)
                    | _ -> Vector2(r.speed*(fst r.orientation),r.speed*(snd r.orientation))
                new VGDLSprite(
                    texture,
                    position=position,
                    orientation=defaultArg direction velocity,
                    speed=r.speed,
                    id=sprite_id,
                    elapsed_time=0,
                    total=r.total,
                    resources=Map.empty,
                    mclass=r.mclass,
                    shootType=r.shootType,
                    cooldown=r.cooldown,
                    probability=
                        (match r.mclass with
                        | Spreader -> r.spreadprob
                        | _ -> r.probability),
                    limit=r.limit,
                    duration=0,
                    ammo=r.ammo,
                    min_ammo=r.min_ammo,
                    ammo_cost=r.ammo_cost
                    )

            yield sprite_id, init
            |]
        |> Array.sortBy (fun (k,v) -> k)
        |> Array.map (fun (k,v) -> v)

    let level_mapping_set =
        tree_rec.level_mapping_set
        |> Map.map (fun k v -> 
            v |> List.toArray 
            |> Array.choose (
                fun x -> 
                match id_map.TryGetValue x with
                | true, v -> Some id_map.[x]
                | false, _ -> 
                    printfn "In LevelMapping there is an invalid mapping. key=%s" x
                    None
                ))

    let termination_set =
        tree_rec.termination_set
        |> List.toArray
        |> Array.map(
            function
            | Timeout x -> 
                let mutable limit = 0
                let mutable win = Lose
                x |> Seq.iter (
                    function
                    | TerminationLimit x -> limit <- x
                    | TerminationWin x -> win <- x
                    | _ -> failwith "Should not be in Timeout."
                    )
                TimeoutTagged(limit*TICK_ADJUSTER,win) // I'll increment the limit here to roughly match the GVGAI library which I think is set to 20fps - roughly 3x slower.
            | SpriteCounter x -> 
                let mutable id = -1
                let mutable limit = 0
                let mutable win = Win
                x |> Seq.iter (
                    function
                    | TerminationSType x -> id <- id_map.[x]
                    | TerminationLimit x -> limit <- x
                    | TerminationWin x -> win <- x
                    | _ -> failwith "Should not be in SpriteCounter."
                    )
                if id = -1 then failwith "A SpriteCounter does not have an stype in TerminationSet."
                SpriteCounterTagged(id,limit,win)
            | MultiSpriteCounter x -> 
                let ids = ResizeArray()
                let mutable limit = 0
                let mutable win = Win
                x |> Seq.iter (
                    function
                    | TerminationNumberedSType(_,x) -> ids.Add id_map.[x]
                    | TerminationLimit x -> limit <- x
                    | TerminationWin x -> win <- x
                    | _ -> failwith "Should not be in SpriteCounter."
                    )
                if ids.Count = 0 then failwith "A MultiSpriteCounter does not have even a single stype in TerminationSet."
                MultiSpriteCounterTagged(ids.ToArray(),limit,win)
            )

    let resource_limits = // Limits for all the ids.
        [|
        for x in tree_map do
            yield x.Key,x.Value |> fst |> fun v -> v.limit
        |]
        |> Array.sortBy (fun (k,_) -> k)
        |> Array.map (fun (_,v) -> v)

    let resource_list = // Those with the Resource class will get printed
        [|
        for x in tree_map do
            let k,v = x.Key,x.Value |> fst
            match v.mclass with
            | Resource -> yield reverse_id_map.[k], k
            | _ -> ()
        |]

    let reverse_singleton_hierarchy = // Filters out non singletons from the reverse hierarchy.
        reverse_hierarchy
        |> Array.map (fun x ->
            x |> Array.tryFind (fun x -> 
                tree_map.[x] |> fst |> fun x -> x.singleton))

    let avatar_id = level_mapping_set.['A'].[0]

    id_map, tree_map, level_mapping_set, tagged_eos, tagged_neos, termination_set, reverse_hierarchy, initializer_map, 
    resource_limits, resource_list, reverse_id_map, reverse_singleton_hierarchy, avatar_id, stepbacks_neos

module VGDLSemantic
// Various tree transformations and error checking to make the raw tree from the parsing step more useful.
// The semantic analysis stage.

#if INTERACTIVE
#load "VGDLParser.fsx"
#endif

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open VGDLParser
open FParsec
open System.Collections.Generic
open System.Linq

let STANDARD_SPEED = 3.0f
let TICK_ADJUSTER = 6

type VGDLSprite =
    struct
    val texture : Texture2D option
    val position : Vector2
    val velocity : Vector2
    val id : int
    val elapsed_time : int
    val total : int
    val orientation : Vector2
    val resources : Map<int, int>
    val mclass : MainClassTypes
    val shootType : int
    val cooldown : int
    val probability : float
    val limit : int
    val duration : int
        
    new(texture,position,velocity,id,elapsed_time,total,orientation,resources,mclass,shootType,cooldown,probability,limit,duration) =
        {
        texture=texture;position=position;velocity=velocity;
        id=id;elapsed_time=elapsed_time;total=total;orientation=orientation;resources=resources;
        mclass=mclass;shootType=shootType;cooldown=cooldown;probability=probability;limit=limit;
        duration=duration;
        }
    end

    /// The default init.
    static member def= // Should be synced with the default record, but no matter.
        new VGDLSprite(None,
            position=Vector2(0.0f,0.0f),
            velocity=Vector2(0.0f,0.0f),
            id= -1,
            elapsed_time=0,
            total=0,
            orientation=Vector2(0.0f,0.0f),
            resources=Map.empty,
            mclass=NoClass,
            shootType=0,
            cooldown=0,
            probability=0.0,
            limit=0,
            duration=0)

// The type that plays the role of a transitioner.
type VGDLMutableSprite =
    struct
    val mutable texture : Texture2D option
    val mutable position : Vector2
    val mutable velocity : Vector2
    val mutable id : int
    val mutable elapsed_time : int
    val mutable total : int
    val mutable orientation : Vector2
    val mutable resources : Map<int, int>
    val mutable mclass : MainClassTypes
    val mutable shootType : int
    val mutable cooldown : int
    val mutable probability : float
    val mutable limit : int
    val mutable duration : int
        
    new(texture,position,velocity,id,elapsed_time,total,orientation,resources,mclass,shootType,cooldown,probability,limit,duration) =
        {
        texture=texture;position=position;velocity=velocity;
        id=id;elapsed_time=elapsed_time;total=total;orientation=orientation;resources=resources;
        mclass=mclass;shootType=shootType;cooldown=cooldown;probability=probability;limit=limit;
        duration=duration;
        }
    end

    static member inline fromIm (x: VGDLSprite) =
        new VGDLMutableSprite(x.texture,x.position,x.velocity,x.id,x.elapsed_time,x.total,x.orientation,x.resources,x.mclass,x.shootType,x.cooldown,x.probability,x.limit,x.duration)

    member inline x.toIm =
        new VGDLSprite(x.texture,x.position,x.velocity,x.id,x.elapsed_time,x.total,x.orientation,x.resources,x.mclass,x.shootType,x.cooldown,x.probability,x.limit,x.duration)

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
    value = 0
    }

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
            orientation = OrientationConstants.DOWN_RIGHT
            color = ColorsConstants.WHITE
            speed = 1.0
            image = ""
            total = System.Int32.MaxValue
            singleton = false
            shrinkfactor = 1.0
            limit = 0
            }

        let sprite_names, hierarchy_map = mapHierarchy tree_rec.sprite_set
        
        let id_map, reverse_id_map = 
            "avatar"::"wall"::sprite_names 
            |> List.toArray
            |> Array.distinct 
            |> Array.rev
            |> Array.mapi (fun i x -> x,i)
            |> fun x -> x.ToDictionary((fun (k,v) -> k),(fun (k,v) -> v), HashIdentity.Structural)
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
                            | MainClass (ShootAvatar | FlakAvatar | MovingAvatar as x) -> 
                                if state.image = "" then
                                    {state with mclass = x; image="avatar"}
                                else
                                    {state with mclass = x}
                            | MainClass x -> {state with mclass = x}
                            | ShootType x -> {state with shootType = id_map.[x]}
                            | Cooldown x -> {state with cooldown = x*TICK_ADJUSTER}
                            | Probability x -> {state with probability = 1.0 - (1.0-x) ** (1.0 / (float TICK_ADJUSTER))}
                            | Orientation (x,y) -> {state with orientation = x,y}
                            | Color (x,y,z) -> {state with color = x,y,z}
                            | Speed x -> {state with speed = x}
                            | Image x -> {state with image = x}
                            | Total x -> {state with total = x}
                            | Singleton x -> {state with singleton = x}
                            | ShrinkFactor x -> {state with shrinkfactor = x}
                            | Limit x -> {state with limit = x}
                            | PhysicsType x -> state // TODO: This will do nothing for now.
                            ) default_record v
                )
            |> Map.toArray |> Array.map (fun (k,v) -> id_map.[k],v) |> Map.ofArray
            |> fun map ->
                map |> Map.map (fun k v -> 
                    match v.mclass with
                    | Spawnpoint -> 
                        if v.shootType = -1 then failwith "Spawnpoints should always have stypes defined."
                        if v.cooldown = 0 then {v with cooldown = map.[v.shootType].cooldown} else v
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
                | Interaction((_,"EOS"),_) -> loop (x::eos) neos xs
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
                    | false, _ ->
                        let t = id_map.Count
                        printfn "Assigning new id %i to resource %s" t x
                        id_map.Add(x,t)
                        {state with resource = t}
                | InteractionLimit x -> {state with limit = x}
                | InteractionStype x -> {state with stype = id_map.[x]}
                | InteractionValue x -> {state with value = x}
            Seq.fold fold_state default_tagrec hashset
               
        function
        | TurnAround -> TurnAroundTagged
        | CollectResource x -> let t = tag_hashset x in CollectResourceTagged(r, t.scoreChange)
        | KillIfFromAbove x -> let t = tag_hashset x in KillIfFromAboveTagged t.scoreChange
        | KillIfOtherHasMore x -> let t = tag_hashset x in KillIfOtherHasMoreTagged(t.resource,t.limit,t.scoreChange)
        | KillSprite x -> let t = tag_hashset x in KillSpriteTagged t.scoreChange
        | TransformTo x -> let t = tag_hashset x in TransformToTagged(t.stype,t.scoreChange)
        | CloneSprite -> CloneSpriteTagged
        | ChangeResource x -> let t = tag_hashset x in ChangeResourceTagged(t.resource,t.value)
        | PullWithIt -> PullWithItTagged
        | KillIfHasLess x -> let t = tag_hashset x in KillIfHasLessTagged(t.resource,t.limit,t.scoreChange)
        | TeleportToExit -> TeleportToExitTagged
        | BounceForward -> BounceForwardTagged

    let interaction_tagger_del l r =
        function
        | StepBack -> if pull_with_it_exists then StepBackAndClearPullWithItTagged else StepBackTagged
        | WrapAround -> WrapAroundTagged
        | ReverseDirection -> ReverseDirectionTagged           

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
        d.ToArray() |> Array.sortBy (fun x -> x.Key) |> Array.map (fun x -> x.Value |> Seq.toArray)
        
    let initializer_map (texture_dict : Dictionary<string, Texture2D>) =
        /// Get the image name of a sprite otherwise it returns ""
        let image_name_of sprite_name = 
            tree_map.[sprite_name] 
            |> fst 
            |> fun x -> x.image
        Map(
            [|
            for p in tree_map do
                let sprite_id,r = p.Key, p.Value |> fst
                let texture = 
                    match texture_dict.TryGetValue (image_name_of sprite_id) with
                    | true, v -> Some v
                    | false, _ -> None
                let velocity = 
                    let speed = r.speed |> float32
                    let v = STANDARD_SPEED
                    match r.mclass with
                    | FlakAvatar -> Vector2(v*speed,0.0f)
                    | Bomber -> Vector2(v*speed,0.0f)
                    | _ -> Vector2(v*speed*(fst r.orientation),v*speed*(snd r.orientation))
                let init position = 
                    new VGDLSprite(
                        texture,
                        position=position,
                        velocity=velocity,
                        id=sprite_id,
                        elapsed_time=0,
                        total=r.total,
                        orientation=Vector2(1.0f,1.0f),
                        resources=Map.empty,
                        mclass=r.mclass,
                        shootType=r.shootType,
                        cooldown=r.cooldown*10,
                        probability=r.probability,
                        limit=r.limit,
                        duration=0
                        )

                yield sprite_id, init
                |])
        |> fun x -> Dictionary(x,HashIdentity.Structural)

    let level_mapping_set =
        tree_rec.level_mapping_set
        |> Map.map (fun k v -> 
            v |> List.toArray 
            |> Array.map (
                fun x -> 
                match id_map.TryGetValue x with
                | true, v -> id_map.[x]
                | false, _ -> failwithf "In LevelMapping there is an invalid mapping. key=%s" x
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

    let resource_list = // Those with resource annotation will get printed
        [|
        for x in tree_map do
            let k,v = x.Key,x.Value |> fst
            match v.mclass with
            | Resource -> yield reverse_id_map.[k], k
            | _ -> ()
        |]

    id_map, tree_map, level_mapping_set, tagged_eos, tagged_neos, termination_set, reverse_hierarchy, initializer_map, resource_limits, resource_list, reverse_id_map

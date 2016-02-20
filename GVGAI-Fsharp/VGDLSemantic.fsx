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

// TODO: textures and queues are not supposed to be a part of the object state. Do something about that.
type VGDLSprite =
    struct
    val texture : Texture2D option
    val queue_del : ResizeArray<InteractionTypesDelayed>
    val queue_im : ResizeArray<InteractionTypesImmediate>
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
        
    new(texture,queue_del,queue_im,position,velocity,id,elapsed_time,total,orientation,resources,mclass,shootType,cooldown,probability,limit,duration) =
        {
        texture=texture;queue_del=queue_del;queue_im=queue_im;position=position;velocity=velocity;
        id=id;elapsed_time=elapsed_time;total=total;orientation=orientation;resources=resources;
        mclass=mclass;shootType=shootType;cooldown=cooldown;probability=probability;limit=limit;
        duration=duration;
        }
    end

    /// The default init.
    static member def=
        new VGDLSprite(None,
            queue_del=ResizeArray(),
            queue_im=ResizeArray(),
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
    val mutable queue_del : ResizeArray<InteractionTypesDelayed>
    val mutable queue_im : ResizeArray<InteractionTypesImmediate>
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
        
    new(texture,queue_del,queue_im,position,velocity,id,elapsed_time,total,orientation,resources,mclass,shootType,cooldown,probability,limit,duration) =
        {
        texture=texture;queue_del=queue_del;queue_im=queue_im;position=position;velocity=velocity;
        id=id;elapsed_time=elapsed_time;total=total;orientation=orientation;resources=resources;
        mclass=mclass;shootType=shootType;cooldown=cooldown;probability=probability;limit=limit;
        duration=duration;
        }
    end

    static member inline fromIm (x: VGDLSprite) =
        new VGDLMutableSprite(x.texture,x.queue_del,x.queue_im,x.position,x.velocity,x.id,x.elapsed_time,x.total,x.orientation,x.resources,x.mclass,x.shootType,x.cooldown,x.probability,x.limit,x.duration)

    member inline x.toIm =
        new VGDLSprite(x.texture,x.queue_del,x.queue_im,x.position,x.velocity,x.id,x.elapsed_time,x.total,x.orientation,x.resources,x.mclass,x.shootType,x.cooldown,x.probability,x.limit,x.duration)
        

type MainRec = 
    {
    sprite_set : SpriteSetTypes list
    termination_set : TerminationSetTypes list 
    interaction_set : InteractionSetTypes list
    level_mapping_set : Map<char,string>
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

    let id_map, tree_map =
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
                | [] -> accum |> List.rev, map
            loop Map.empty [] spriteset

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
            total = 0
            singleton = false
            shrinkfactor = 1.0
            limit = 0
            }

        let sprite_names, hierarchy_map = mapHierarchy tree_rec.sprite_set
        let id_map = sprite_names |> Set |> fun x -> x.Add("avatar").Add("wall") |> Set.toArray |> Array.mapi (fun i x -> x,i) |> Map.ofArray

        let flat_spriteset = 
            flattenSpriteSet tree_rec.sprite_set
            |> Map.map (fun k v ->
                    Seq.fold (
                        fun state v -> 
                            match v with
                            | MainClass (ShootAvatar | FlakAvatar | MovingAvatar as x) -> {state with mclass = x; image="avatar"}
                            | MainClass x -> {state with mclass = x}
                            | ShootType x -> {state with shootType = id_map.[x]}
                            | Cooldown x -> {state with cooldown = x}
                            | Probability x -> {state with probability = x}
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

        let tree_map = 
            let t1 = flat_spriteset |> Map.toArray
            let t2 = hierarchy_map |> Map.toArray
            Array.map2 (fun (k1,v1) (k2,v2) -> if k1 = k2 then k1,(v1,v2) else failwith "Maps are not ordered") t1 t2
            |> Map.ofArray
            |> fun x -> if x.ContainsKey("wall") = false then x.Add("wall",({default_record with image="wall"},[])) else x
            |> fun x -> if x.ContainsKey("avatar") = false then x.Add("avatar",(default_record,[])) else x
            
        id_map, tree_map

    let id_hierarchy_map = 
        tree_map |> Map.map ( fun  parent (_,children) -> parent::children |> List.toArray |> Array.map(fun x -> id_map.[x]))

    let eos, neos =
        let rec loop (eos: _ list) (neos: _ list)=
            function
            | x::xs ->
                match x with
                | Interaction((_,"EOS"),_) -> loop (x::eos) neos xs
                | Interaction _ -> loop eos (x::neos) xs
                | _ -> failwith "Not supposed to be called on an processed set."
            | [] -> eos,neos
        loop [] [] (tree_rec.interaction_set)

    let tagged_eos = 
        let buf = Array.init tree_map.Count (fun _ -> ResizeArray())
        eos 
        |> List.iter ( function 
            | Interaction((l,r),attrs) -> 
                for x in id_hierarchy_map.[l] do
                    buf.[x].Add(
                        attrs
                        |> List.map (
                                fun x ->
                                match x with
                                | ImmediateInteraction(TransformTo x) -> ImmediateInteraction <| TransformToTagged (id_map.[x])
                                | ImmediateInteraction(KillIfOtherHasMore (x,y)) -> ImmediateInteraction <| KillIfOtherHasMoreTagged (id_map.[x],y)
                                | _ -> x
                                ))
            | _ -> failwith "Interaction only."
            )
        buf 
        |> Array.map (fun x -> x.ToArray() |> Array.collect (fun x -> x |> List.toArray)) // Plain arrays are faster than ResizeArrays for iterating over them.
        |> Array.map (
            fun x -> 
                x |> Array.choose (function DelayedInteraction x -> Some x | _ -> None),
                x |> Array.choose (function ImmediateInteraction x -> Some x | _ -> None))

    let tagged_neos = 
        let buf = Array2D.init tree_map.Count tree_map.Count (fun _ _ -> ResizeArray())
        neos 
        |> List.iter (
            function 
            | Interaction((l,r),attrs) -> 
                for x in id_hierarchy_map.[l] do
                    for y in id_hierarchy_map.[r] do
                        buf.[x,y].Add(
                            attrs
                            |> List.map (
                                    fun x ->
                                    match x with
                                    | ImmediateInteraction(TransformTo x) -> ImmediateInteraction <| TransformToTagged (id_map.[x])
                                    | ImmediateInteraction(KillIfOtherHasMore (x,y)) -> ImmediateInteraction <| KillIfOtherHasMoreTagged (id_map.[x],y)
                                    | _ -> x
                                    ))
                | _ -> failwith "Interaction only."
                )
        buf 
        |> Array2D.map (fun x -> x.ToArray() |> Array.collect (fun x -> x |> List.toArray)) // Plain arrays are faster than ResizeArrays for iterating over them.
        |> Array2D.map (
            fun x -> 
                x |> Array.choose (function DelayedInteraction x -> Some x | _ -> None),
                x |> Array.choose (function ImmediateInteraction x -> Some x | _ -> None))

    let reverse_hierarchy = // Computes the reverse hierarchy from children to parents. Needed for TerminationSet counters.
        let d = Dictionary<int,HashSet<int>>(HashIdentity.Structural)
        for x in tree_map do
            let parent,children = 
                (x.Key,x.Value |> snd)
                |> fun (k,v) ->
                    id_map.[k],
                    v |> List.toArray |> Array.map (fun x -> id_map.[x])

            match d.TryGetValue parent with
            | true, dh -> ()
            | false, _ -> d.Add(parent, HashSet([|parent|],HashIdentity.Structural))

            for self in children do
                match d.TryGetValue self with
                | true, dh -> dh.Add parent |> ignore
                | false, _ -> d.Add(self, HashSet([|self;parent|],HashIdentity.Structural))
        let d' = Dictionary<int,int[]>(HashIdentity.Structural)
        for x in d do d'.Add(x.Key,x.Value |> Seq.toArray)
        d' |> Seq.map (fun x -> x.Value) |> Seq.toArray

        
    let initializer_map (texture_dict : Dictionary<string, Texture2D>) =
        /// Get the image name of a sprite otherwise it returns ""
        let image_name_of sprite_name = 
            tree_map.[sprite_name] 
            |> fst 
            |> fun x -> x.image
        Map(
            [|
            for p in tree_map do
                let sprite_name,r = p.Key, p.Value |> fst
                let texture = 
                    match texture_dict.TryGetValue (image_name_of sprite_name) with
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
                        queue_del=ResizeArray(),
                        queue_im=ResizeArray(),
                        position=position,
                        velocity=velocity,
                        id=id_map.[sprite_name],
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

                yield id_map.[sprite_name], init
                |])
        |> fun x -> Dictionary(x,HashIdentity.Structural)

    let level_mapping_set =
        try
            tree_rec.level_mapping_set
            |> Map.map (fun k v -> id_map.[v])
        with
            | :? KeyNotFoundException as x ->
                printfn "In LevelMapping there is an invalid mapping."
                reraise()

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
                TimeoutTagged(limit,win)
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

    id_map, tree_map, level_mapping_set, tagged_eos, tagged_neos, termination_set, reverse_hierarchy, initializer_map

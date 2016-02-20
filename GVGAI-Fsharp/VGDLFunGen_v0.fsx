// It is only slightly incorrect, but it lacks flexibility.
// I need to make some major changes to it so I will back it up here as v0.

// Ironically, even though it lacks (triple) buffering, it might be more suitable for 
// AI tasks than the version I am going to make as it is more efficient.

// I also intend to add a new collision detection scheme that will work in O(n).

#if INTERACTIVE
#r @"C:\Users\Marko\Documents\Visual Studio 2015\Projects\FSharpx.Collections\src\FSharpx.Collections\bin\Release\FSharpx.Collections.dll"
#load "VGDLSemantic.fsx"
#endif
open VGDLParser
open VGDLSemantic
open Microsoft.FSharp.Collections

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System
open System.IO
open System.Collections
open System.Collections.Generic

let rng = Random()
let noise = 0.01 // Noise can be used to ameliorate round-off errors in enemy movement.

let sprite_list =
    Directory.GetFiles (__SOURCE_DIRECTORY__ + @"\Sprites")
    |> Seq.toArray
    |> Array.mapi (fun i x -> ((x.Split [|'\\'|] |> Array.last).Split [|'.'|]).[0],i)

let font_list =
    Directory.GetFiles (__SOURCE_DIRECTORY__ + @"\Fonts")
    |> Seq.toArray
    |> Array.mapi (fun i x -> ((x.Split [|'\\'|] |> Array.last).Split [|'.'|]).[0],i)

let SPRITE_WIDTH, SPRITE_HEIGHT = 20,20
let GRID_WIDTH, GRID_HEIGHT = 5.f, 5.f
let standard_velocity = 3

type VGDLSprite =
    {
    queue : ResizeArray<InteractionTypes>
    texture : Texture2D option
    mutable position : Vector2
    resources : Dictionary<int, int>
    id : int
    effect : VGDLSprite -> int -> unit // The unary effects
    message_processor : VGDLSprite -> int -> unit
    }

    // Continuous
//    member inline sprite.rect = Rectangle(int sprite.position.X, int sprite.position.Y, SPRITE_WIDTH, SPRITE_HEIGHT)
//    member inline sprite.rect_lower_half_horizontal = Rectangle(int sprite.position.X, int sprite.position.Y+10, SPRITE_WIDTH, SPRITE_HEIGHT-10) // For horizontal cut intersections.
    member inline sprite.grid = Vector2((truncate <| sprite.position.X / GRID_WIDTH) * GRID_WIDTH, (truncate <| sprite.position.Y / GRID_HEIGHT) * GRID_HEIGHT)
    member inline sprite.rect = 
        let position = sprite.grid
        Rectangle(int position.X, int position.Y, SPRITE_WIDTH, SPRITE_HEIGHT)
    member inline sprite.rect_lower_half_horizontal = 
        let position = sprite.grid
        Rectangle(int sprite.position.X, int sprite.position.Y+10, SPRITE_WIDTH, SPRITE_HEIGHT-10) // For horizontal cut intersections.
    member inline sprite.draw (spriteBatch: SpriteBatch) = 
        match sprite.texture with
        | Some texture -> spriteBatch.Draw(texture, sprite.grid, Color.White)
        | None -> ()

type VGDLSpriteFont =
    {
    font : SpriteFont
    font_position : Vector2
    }

    member inline sprite.draw (spriteBatch: SpriteBatch) (score: string) = 
        spriteBatch.DrawString(sprite.font,score,sprite.font_position,Color.AliceBlue)

/// The MonoGame engine class. Pass the VGDL game description and level as constructor inputs.
type VGDLGame(gameDesc: string, levelDesc: string, global_speed: float32, outcome_ref) as this =
    inherit Game()

    let id_map, tree_map, level_mapping, tagged_eos, tagged_neos, termination_set, reverse_hierarchy =
        runSematic gameDesc

    let mutable tick = 0
    let mutable score = 0
    let mutable score_string = "Score: 0"

    let level_split = levelDesc.Split '\n' |> Array.map (fun x -> x.TrimEnd())

    let globalTexure2dDict = Dictionary(HashIdentity.Structural)
    let globalSpriteFontDict = Dictionary(HashIdentity.Structural)

    // Create a new SpriteBatch, which can be used to draw textures.
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch> 

    do this.Content.RootDirectory <- __SOURCE_DIRECTORY__

    let graphics = new GraphicsDeviceManager(this)
    do graphics.PreferredBackBufferWidth <- level_split |> Array.map (fun x -> x.Length*SPRITE_WIDTH) |> Array.max
    do graphics.PreferredBackBufferHeight <- level_split.Length*SPRITE_HEIGHT // The program depends on sprites being 20x20 in dimension.

    let BACKBUFFER_WIDTH_F = graphics.PreferredBackBufferWidth |> float32
    let BACKBUFFER_HEIGHT_F = graphics.PreferredBackBufferHeight |> float32

    do this.IsMouseVisible <- true

    let sprites = ResizeArray<VGDLSprite>()
    let sprites = ResizeArray<VGDLSprite>()

    let printers = ResizeArray<VGDLSpriteFont>()

    // In F# forward declaration can be done using a function reference.
    // As the effect functions need the ability to create other objects, they need to access the initializer map,
    // but the initializer map comes after the effects. This is the best solution.
    let (forward_initializer_map : Map<int, Vector2 -> VGDLSprite> ref) = ref Map.empty

    /// For the singleton contraint, I will have to make a manager.
    /// This one manages additions to the sprites array.
    let sprite_manager_add, sprite_manager_process =
        let mutable queue = ResizeArray()
        let singleton_sprites = 
            tree_map 
            |> Map.filter (fun k v -> (fst v).singleton)
            |> Map.toArray 
            |> Array.map (fun (k,(v,_)) -> id_map.[k])
            |> fun x -> HashSet(x,HashIdentity.Structural)
        let add id pos = queue.Add (id,pos)
        let process_queue () = 
            queue 
            |> ResizeArray.partition(fun (x,_) -> singleton_sprites.Contains x) // Paritions the queue into singleton and nonsingleton sprites.
            |> fun (s,nons) -> 
                if s.Count > 0 then
                    s   |> ResizeArray.distinctBy (fun x -> fst x) // Pares down the singletons if more than one is present in the array.
                        |> ResizeArray.filter (fun (id,_) -> sprites |> ResizeArray.exists(fun sprite -> id = sprite.id) |> not) // Filters out singletons present in the sprites array.
                        |> ResizeArray.iter (fun (id,pos) -> let f = forward_initializer_map.Value.[id] in sprites.Add(f pos)) // Finally adds those that need to be added.
                nons |> ResizeArray.iter (fun (id,pos) -> let f = forward_initializer_map.Value.[id] in sprites.Add(f pos)) // Non singleton sprites are just added without limit.
                
            queue.Clear()
            
        add, process_queue

    let killsprite_manager_add, killsprite_manager_process =
        let queue = ResizeArray()
        let killsprite_manager_add i = queue.Add i
        let killsprite_manager_process () = // Queue based removal at indices. Identical to a hypothetical sprites.RemoveIndices(queue)
            let rec loop i j k (queue: ResizeArray<_>) =
                if k < queue.Count then
                    if j <> queue.[k] then
                        sprites.[i] <- sprites.[j]
                        loop (i+1) (j+1) k queue
                    else
                        loop i (j+1) (k+1) queue
                else if j < sprites.Count then 
                    sprites.[i] <- sprites.[j]
                    loop (i+1) (j+1) k queue
                else i,j-i
            if queue.Count > 0 then 
                // A sort is needed here because there are some ridiculous edge cases where the ids do not get added in the correct order.
                let cut_off = loop 0 0 0 (ResizeArray.distinct queue |> fun x -> x.Sort(); x) 
                sprites.RemoveRange(cut_off)
                queue.Clear()

        killsprite_manager_add, killsprite_manager_process

    let binary_effect_manager_process =
        let get_collisions () =
            [|
            for i=0 to sprites.Count-1 do
                for j=i+1 to sprites.Count-1 do
                    let a,b = sprites.[i], sprites.[j]
                    let intersect = Rectangle.Intersect(a.rect, b.rect)
                    if intersect.Size <> Point(0) then
                        yield i,j,a,b |]

        let get_eos_boundary_crossings () =
            /// Checks if the sprite is outside the window boundary.
            let inline is_sprite_outside_window (sprite: VGDLSprite) =
                /// Checks if the sprite is outside the horizontal window boundary.
                let inline is_sprite_outside_window_horizontal (sprite: VGDLSprite) =
                    sprite.position.X < 0.0f || (sprite.position.X + (float32 <| SPRITE_WIDTH) > BACKBUFFER_WIDTH_F)
                /// Checks if the sprite is outside the vertical window boundary.
                let inline is_sprite_outside_window_vertical (sprite: VGDLSprite) =
                    sprite.position.Y < 0.0f || (sprite.position.Y + (float32 <| SPRITE_HEIGHT) > BACKBUFFER_HEIGHT_F)

                is_sprite_outside_window_horizontal sprite || is_sprite_outside_window_vertical sprite
            [|
            for i=0 to sprites.Count-1 do
                let a = sprites.[i]
                if is_sprite_outside_window a then yield i,a
                |]

        let inline interaction_function messages i (ra: VGDLSprite) (rb: _ option) = // This one sensds messages to the queue
            for x in messages do
                match x with
                | KillSprite -> killsprite_manager_add i
                | ScoreChange x -> score <- score+x; score_string <- sprintf "Score: %i" score
                | MoveTo _ | Spawn _ | TurnAround | StepBack -> ra.queue.Add(x)
                | CollectResource _ -> rb.Value.queue.Add(CollectResource ra.id)
                | KillIfFromAbove x -> 
                    let sec = Rectangle.Intersect(ra.rect,rb.Value.rect_lower_half_horizontal)
                    if sec.Width-standard_velocity > sec.Height then 
                        score <- score+x; score_string <- sprintf "Score: %i" score
                        killsprite_manager_add i
                | TransformTo _ | KillIfOtherHasMore _ -> failwith "Should be tagged."
                | KillIfOtherHasMoreTagged (resource,limit) -> 
                    match rb.Value.resources.TryGetValue(resource) with
                    | true, v -> if v >= limit then killsprite_manager_add i
                    | false, _ -> if 0 >= limit then killsprite_manager_add i
                | TransformToTagged x -> killsprite_manager_add i; sprite_manager_add x ra.position

        let collision_function_generator (TaggedInteraction((a,b),l)) = // These two generators generate closures for each of the interaction set constraints
            let triggers = HashSet(b |> Array.collect (fun x -> a |> Array.map(fun y -> y,x)),HashIdentity.Structural)
            let messages = l |> List.toArray

            fun (collisions: _[]) ->
                for i,j,a,b in collisions do
                    if triggers.Contains(a.id,b.id) then interaction_function messages i a (Some b)
                    else if triggers.Contains(b.id,a.id) then interaction_function messages j b (Some a)

        let eos_function_generator (TaggedInteractionEOS(a,l)) =
            let triggers = HashSet(a,HashIdentity.Structural)
            let messages = l |> List.toArray
            fun (crossings: _[]) ->
                for i,a in crossings do
                    if triggers.Contains(a.id) then interaction_function messages i a None

        let collision_functions = [|
            for interaction in tagged_neos do
                yield collision_function_generator interaction |]

        let eos_functions = [|
            for interaction in tagged_eos do
                yield eos_function_generator interaction |]

        fun () ->
            let collisions = get_collisions()
            for f in collision_functions do f collisions
            let crossings = get_eos_boundary_crossings()
            for f in eos_functions do f crossings

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
    
    let effect (r: AttributeTypesRecord) =
        let mutable prev_position = Vector2(0.0f,0.0f)
        let mutable velocity = 
            let speed = r.speed |> float32
            let v = float32 standard_velocity
            match r.mclass with
            | FlakAvatar -> Vector2(v*speed,0.0f*speed)
            | ShootAvatar -> Vector2(v*speed,v*speed)
            | Bomber -> Vector2(v*speed,0.0f)
            | _ -> Vector2(v*speed*(fst r.orientation),v*speed*(snd r.orientation))
        let mutable elapsed_time = 0
        let mutable total = r.total
        let mutable orientation = DOWN

        /// Moves the sprite in the direction of its velocity.
        let inline passiveMoveEffect sprite i =
            let inline add_opt_noise x = // Noise can be used to ameliorate unneven enemy movement.
                if noise <> 0.0 then x*(float32 <| noise*rng.NextDouble()) else 0.0f
            prev_position <- sprite.position
            let vx = velocity.X
            let vy = velocity.Y
            sprite.position <- Vector2(sprite.position.X + vx + add_opt_noise vx,sprite.position.Y + vy + add_opt_noise vy)

        let inline avatarMoveEffect sprite i =
            let k = Keyboard.GetState()
            let x =
                if k.IsKeyDown(Keys.Left) then 
                    orientation <- LEFT
                    -1.0f*velocity.X
                else if k.IsKeyDown(Keys.Right) then 
                    orientation <- RIGHT
                    1.0f*velocity.X
                else 0.0f
            let y =
                if k.IsKeyDown(Keys.Down) then 
                    orientation <- DOWN
                    1.0f*velocity.Y
                else if k.IsKeyDown(Keys.Up) then 
                    orientation <- UP
                    -1.0f*velocity.Y
                else 0.0f
            
            Vector2(sprite.position.X + x,sprite.position.Y + y) |> MoveTo |> sprite.queue.Add

        let randomMove =
            let v = standard_velocity |> float32
            let mutable move = RIGHT
            let mutable duration = 0 // The random NPC works by executing a random move for a random duration.
            fun sprite i ->
                duration <- duration-1
                if duration < 0 then
                    move <- ORIENTATIONS.[rng.Next(4)]
                    duration <- rng.Next(2,10)
                sprite.position+move*v |> MoveTo |> sprite.queue.Add

        let inline timedShoot sprite i =
            elapsed_time <- elapsed_time + 1
            let cool = r.cooldown*10
            if elapsed_time > cool then
                let k = Keyboard.GetState()
                if k.IsKeyDown(Keys.Space) then
                    elapsed_time <- 0
                    (r.shootType, sprite.position) |> Spawn |> sprite.queue.Add

        let inline timedOrientedShoot sprite i =
            elapsed_time <- elapsed_time + 1
            let cool = r.cooldown*10
            if elapsed_time > cool then
                let k = Keyboard.GetState()
                if k.IsKeyDown(Keys.Space) then
                    elapsed_time <- 0
                    (r.shootType, sprite.position + orientation*20.0f) |> Spawn |> sprite.queue.Add

        let inline enemyShoot sprite i =
            elapsed_time <- elapsed_time + 1
            let cool = r.cooldown*10
            if elapsed_time > cool && rng.NextDouble() < r.probability then
                elapsed_time <- 0
                (r.shootType, sprite.position) |> Spawn |> sprite.queue.Add

        let inline enemySpawn sprite i =
            elapsed_time <- elapsed_time + 1
            let cool = r.cooldown*10
            if total > 0 then
                if elapsed_time > cool then
                    elapsed_time <- 0
                    total <- total-1
                    (r.shootType, sprite.position) |> Spawn |> sprite.queue.Add
            else killsprite_manager_add i // Add self to the termination queue when spawns left runs out.

        let inline flicker sprite i =
            elapsed_time <- elapsed_time+1
            let cool = r.limit*10
            if elapsed_time > cool then
                killsprite_manager_add i // Add self to the termination queue when spawns left runs out.

        let inline messageProcessor sprite i = // Add sprite and kill sprite messages are processed separately.
            for m in sprite.queue do
                match m with
                | Spawn(id,pos) ->
                    sprite_manager_add id pos
                | MoveTo pos ->
                    prev_position <- sprite.position
                    sprite.position <- pos
                | TurnAround ->
                    sprite.position.Y <- sprite.position.Y + (SPRITE_HEIGHT |> float32)
                    velocity <- Vector2(-velocity.X,-velocity.Y)
                | StepBack ->
                    sprite.position <- prev_position
                | CollectResource id ->
                    match sprite.resources.TryGetValue(id) with
                    | true, v -> 
                        sprite.resources.[id] <- v+1
                    | false, _ -> 
                        sprite.resources.[id] <- 1
                | _ -> failwith "Interaction not implemented yet."
            sprite.queue.Clear()

        match r.mclass with
        | ShootAvatar ->
            fun sprite i ->
                avatarMoveEffect sprite i
                timedOrientedShoot sprite i
        | FlakAvatar -> 
            fun sprite i ->
                avatarMoveEffect sprite i
                timedShoot sprite i
        | Missile -> 
            fun sprite i ->
                passiveMoveEffect sprite i
        | Bomber ->
            fun sprite i ->
                passiveMoveEffect sprite i
                enemyShoot sprite i
        | Spawnpoint ->
            fun sprite i ->
                enemySpawn sprite i
        | Flicker ->
            fun sprite i ->
                flicker sprite i
        | RandomNPC ->
            fun sprite i ->
                randomMove sprite i
        | _ -> fun sprite i -> ()

        |> fun effect_function -> effect_function, messageProcessor

    let initializer_map =
        /// Get the image name of a sprite otherwise it returns ""
        let image_name_of sprite_name = 
            tree_map.[sprite_name] 
            |> fst 
            |> fun x -> x.image
        Map(
            [|
            for p in tree_map do
                let sprite_name,v = p.Key, p.Value |> fst
                let init position = 
                    let eff_function, message_processor =
                            tree_map.[sprite_name] 
                            |> fst
                            |> ( fun x -> effect x)
                    {
                    queue = ResizeArray()
                    texture = 
                        match globalTexure2dDict.TryGetValue(image_name_of sprite_name) with
                        | true, x -> Some x
                        | false, _ -> None
                    position = position
                    id = id_map.[sprite_name]
                    effect = eff_function                
                    message_processor = message_processor
                    resources = Dictionary()
                    }
                yield id_map.[sprite_name], init
                |])

    do forward_initializer_map := initializer_map

    let terminators_manager_process =
        let ter = // Do some preprocessing for easier iterating later on.
            termination_set 
            |> List.toArray
            |> Array.map (
                function
                | SpriteCounter(name,limit,win) -> SpriteCounterTagged(id_map.[name],limit, if win then Win else Lose)
                | MultiSpriteCounter(names,limit,win) -> MultiSpriteCounterTagged(names |> List.toArray |> Array.map (fun name -> id_map.[name]),limit, if win then Win else Lose)
                | x -> x
                )

        let get_counts() =
            let c = Array.zeroCreate id_map.Count
            for x in sprites do
                let id = x.id
                for x in reverse_hierarchy.[id] do
                    c.[x] <- c.[x]+1
            c

        fun () ->
            let c = get_counts()
            let rec loop i =
                if i < ter.Length then
                    match ter.[i] with
                    | SpriteCounterTagged(id,limit,win_or_loss) ->
                        if c.[id] = limit then win_or_loss
                        else loop (i+1)
                    | MultiSpriteCounterTagged(ids,limit,win_or_loss) ->
                        let rec test i =
                            if i < ids.Length then
                                if c.[ids.[i]] = limit then test (i+1)
                                else false
                            else true
                        if test 0 then win_or_loss
                        else loop (i+1)
                    | Timeout _ -> failwith "Timeout not implemented!"
                    | _ -> failwith "Can't touch this."
                else Continue
            loop 0

    override this.Initialize() = base.Initialize()

    override this.LoadContent() = 
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)
        for x,i in sprite_list do // Add textures to the global dict.
            globalTexure2dDict.Add(x,this.Content.Load<Texture2D>(@"Sprites\"+x))
        for x,i in font_list do // Add sprite fonts to the global dict.
            globalSpriteFontDict.Add(x,this.Content.Load<SpriteFont>(@"Fonts\"+x))

        for i=0 to level_split.Length-1 do // Creates the level by initializing the spites.
            for j=0 to level_split.[i].Length-1 do
                let c = level_split.[i].[j]
                if c <> ' ' then
                    let sprite_name = c 
                                    |> level_mapping.TryFind 
                                    |> function Some x -> x 
                                              | None -> failwithf "No key for %c found at line %i, column %i!" (level_split.[i].[j]) (i+1) (j+1)

                    let x,y = j*SPRITE_WIDTH,i*SPRITE_HEIGHT
                    let position = Vector2(float32 x, float32 y)

                    sprite_manager_add id_map.[sprite_name] position 

        { // Make a printer.
        font = globalSpriteFontDict.Values |> Seq.toArray |> fun x -> x.[0]
        font_position = Vector2(50.0f,50.0f)
        } |> printers.Add

        
    override this.UnloadContent() =
        for x in globalTexure2dDict.Values do x.Dispose()
        globalTexure2dDict.Clear()
        globalSpriteFontDict.Clear()

    override this.Update(gameTime) = 
        if GamePad.GetState(PlayerIndex.One).Buttons.Back = ButtonState.Pressed || Keyboard.GetState().IsKeyDown(Keys.Escape)
        then this.Exit()

        // Local Effects
        for i=0 to sprites.Count-1 do let sprite = sprites.[i] in sprite.effect sprite i
        // Message processing
        for i=0 to sprites.Count-1 do let sprite = sprites.[i] in sprite.message_processor sprite i
        // Global effects
        sprite_manager_process() // This one is needed for the singleton contraint. Processes sprite addition.
        binary_effect_manager_process() // The one sends messages to other managers based on the InteractionSet
        // Individial interactions.
        killsprite_manager_process() // This one processes the killSprite messages.
        // Message processing
        for i=0 to sprites.Count-1 do let sprite = sprites.[i] in sprite.message_processor sprite i
        // Terminators
        terminators_manager_process()
        |> fun x -> 
            match x with
            | Continue -> ()
            | _ -> 
                outcome_ref := score, x
                printfn "The score is: %i. Outcome: %A." score x
                this.Exit()

        base.Update(gameTime)

    override this.Draw(gameTime) = 
        this.GraphicsDevice.Clear(Color.CornflowerBlue)

        spriteBatch.Begin()

        for sprite in sprites do sprite.draw spriteBatch
        for printer in printers do printer.draw spriteBatch score_string

        spriteBatch.End()
        base.Draw(gameTime)

let aliens_spec = """BasicGame
	SpriteSet
		sword > Flicker color=LIGHTGRAY limit=1 singleton=True img=sword.png
		dirt > Immovable color=BROWN img=dirt.png
		exitdoor > Door color=GREEN img=door.png
		diamond > Resource color=YELLOW limit=10 shrinkfactor=0.75 img=diamond.png
		boulder > Missile orientation=DOWN color=GRAY speed=0.2 img=boulder.png
		moving >
			avatar  > ShootAvatar   stype=sword img=avatar.png
			enemy > RandomNPC
				crab > color=RED img=camel.png
				butterfly > color=PINK img=butterfly.png
	LevelMapping
		. > dirt
		E > exitdoor
		o > boulder
		x > diamond
		c > crab
		b > butterfly
	InteractionSet
		dirt avatar > killSprite
		dirt sword  > killSprite
		diamond avatar > collectResource
		diamond avatar > killSprite scoreChange=2
		moving wall > stepBack
		moving boulder > stepBack
		avatar boulder > killIfFromAbove scoreChange=-1
		avatar butterfly > killSprite scoreChange=-1
		avatar crab > killSprite scoreChange=-1
		boulder dirt > stepBack
		boulder wall > stepBack
		boulder diamond > stepBack
		boulder boulder > stepBack
		enemy dirt > stepBack
		enemy diamond > stepBack
		crab butterfly > killSprite
		butterfly crab > transformTo stype=diamond scoreChange=1
		exitdoor avatar > killIfOtherHasMore resource=diamond limit=9

	TerminationSet
		SpriteCounter stype=avatar limit=0 win=False
		SpriteCounter stype=exitdoor limit=0 win=True"""    

let aliens_text = """wwwwwwwwwwwwwwwwwwwwwwwwww
wx.......o.o.....oxo...oow
w........................w
wooo......o...... ....w..w
w......x....wwwwx x.oow..w
wb ...co.....o........woxw
w  ....x....Ao....o...woxw
w...o.xx.o......o..xoxx..w
wc  .....x..ooxxo ....w..w
w........x........o..o...w
w   ..E..........b     ..w
w....xxx....o....o.oxoo.ow
wwwwwwwwwwwwwwwwwwwwwwwwww"""

let outcome = ref (0,Continue)
let g = new VGDLGame(aliens_spec,aliens_text,1.0f,outcome)
g.Run()

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
let noise = 0.00 // Noise can be used to ameliorate round-off errors in enemy movement. Set it to 0.01.

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


type VGDLSprite with
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
    struct
    val font : SpriteFont
    val font_position : Vector2
    new(font,font_position) = {font=font;font_position=font_position}
    end

    member inline sprite.draw (spriteBatch: SpriteBatch) (score: string) = 
        spriteBatch.DrawString(sprite.font,score,sprite.font_position,Color.AliceBlue)

/// The MonoGame engine class. Pass the VGDL game description and level as constructor inputs.
type VGDLGame(gameDesc: string, levelDesc: string, outcome_ref) as this =
    inherit Game()

    let id_map, tree_map, level_mapping, tagged_eos, tagged_neos, termination_set, reverse_hierarchy, initializer_map_create =
        runSematic gameDesc

    let mutable tick = 0
    let mutable score = 0
    let mutable score_string = "Score: 0"

    let level_split = levelDesc.Split '\n' |> Array.map (fun x -> x.TrimEnd())

    let globalTexure2dDict = Dictionary(HashIdentity.Structural)
    let globalSpriteFontDict = Dictionary(HashIdentity.Structural)

    let initializer_map = lazy initializer_map_create globalTexure2dDict

    // Create a new SpriteBatch, which can be used to draw textures.
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch> 

    do this.Content.RootDirectory <- __SOURCE_DIRECTORY__

    let graphics = new GraphicsDeviceManager(this)
    do graphics.PreferredBackBufferWidth <- level_split |> Array.map (fun x -> x.Length*SPRITE_WIDTH) |> Array.max
    do graphics.PreferredBackBufferHeight <- level_split.Length*SPRITE_HEIGHT // The program depends on sprites being 20x20 in dimension.

    let BACKBUFFER_WIDTH_F = graphics.PreferredBackBufferWidth |> float32
    let BACKBUFFER_HEIGHT_F = graphics.PreferredBackBufferHeight |> float32

    do this.IsMouseVisible <- true

    // As a design choice, the entire state of the game resides in these triple buffered arrays.
    // The structs are unrolled neatly in blocks of contiguous memory for memory coalescing and fast iteration over them.
    // As this version is functional and the structs cannot be modified, only replaced inside the arrays.

    // That is not particularly functional, but it seems like a decent tradeoff.

    // TODO. Replace these flat arrays with ResizeArrays.
    // I somehow got lucky this time and got these to work great, but flat arrays are not worth it even if they are faster.
    let mutable sprites3, sprite_count_new = Array.create 10000 (VGDLSprite.def), 0
    let sprites2 = Array.create 10000 (VGDLSprite.def) // Intermediate for unary effects.
    let mutable sprites1, sprite_count = Array.create 10000 (VGDLSprite.def), 0 

    let swap_sprites() =
        let t = sprites1
        sprites1 <- sprites3
        sprites3 <- t
        let c = sprite_count
        sprite_count <- sprite_count_new
        sprite_count_new <- c

    let mutable printer = VGDLSpriteFont()

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
        let add id (pos: Vector2) = queue.Add (id,pos)
        let process_queue () = 
            if queue.Count > 0 then
                queue 
                |> ResizeArray.partition(fun (x,_) -> singleton_sprites.Contains x) // Paritions the queue into singleton and nonsingleton sprites.
                |> fun (s,nons) -> 
                    if s.Count > 0 then
                        s   |> ResizeArray.distinctBy (fun x -> fst x) // Pares down the singletons if more than one is present in the array.
                            |> ResizeArray.filter (fun (id,_) -> sprites3.[0..sprite_count_new-1] |> Array.exists(fun sprite -> id = sprite.id) |> not) // Filters out singletons present in the sprites array.
                            |> ResizeArray.iter (fun (id,pos) -> 
                                let f = initializer_map.Value.[id] 
                                sprites3.[sprite_count_new] <- f pos
                                sprite_count_new <- sprite_count_new+1) // Finally adds those that need to be added.
                    
                    nons 
                    |> ResizeArray.iter (fun (id,pos) -> 
                        let f = initializer_map.Value.[id] 
                        sprites3.[sprite_count_new] <- f pos
                        sprite_count_new <- sprite_count_new+1
                        ) // Non singleton sprites are just added without limit.
                
                queue.Clear()
            
        add, process_queue


    let killsprite_manager_add, killsprite_manager_process =
        let queue = ResizeArray()
        let killsprite_manager_add i = queue.Add i
        let killsprite_manager_process () = // Queue based removal at indices. Identical to a hypothetical sprites.RemoveIndices(queue)
            if queue.Count > 0 then 
                queue.Sort()
                for i=queue.Count-1 downto 0 do
                    let m = queue.[i]
                    sprite_count_new <- sprite_count_new-1
                    sprites3.[m] <- sprites3.[sprite_count_new]
                queue.Clear()

        killsprite_manager_add, killsprite_manager_process

    let unary_transition i =
        let mutable ns = VGDLMutableSprite.fromIm sprites1.[i]

        let inline avatarMoveEffect() =
            let k = Keyboard.GetState()
            let x =
                if k.IsKeyDown(Keys.Left) then 
                    ns.orientation <- LEFT
                    -1.0f*ns.velocity.X
                else if k.IsKeyDown(Keys.Right) then 
                    ns.orientation <- RIGHT
                    1.0f*ns.velocity.X
                else 0.0f
            let y =
                if k.IsKeyDown(Keys.Down) then 
                    ns.orientation <- DOWN
                    1.0f*ns.velocity.Y
                else if k.IsKeyDown(Keys.Up) then 
                    ns.orientation <- UP
                    -1.0f*ns.velocity.Y
                else 0.0f
            
            ns.position <- Vector2(ns.position.X + x,ns.position.Y + y)

        /// Moves the sprite in the direction of its velocity.
        let inline passiveMoveEffect() =
            let inline add_opt_noise x = // Noise can be used to ameliorate unneven enemy movement.
                if noise <> 0.0 then x*(float32 <| noise*rng.NextDouble()) else 0.0f
            let vx = ns.velocity.X
            let vy = ns.velocity.Y
            ns.position <- Vector2(ns.position.X + vx + add_opt_noise vx,ns.position.Y + vy + add_opt_noise vy)

        let randomMove() =
            let v = STANDARD_SPEED
            ns.duration <- ns.duration-1
            if ns.duration < 0 then
                ns.orientation <- ORIENTATIONS.[rng.Next(4)]
                ns.duration <- rng.Next(2,10)
            ns.position <- ns.position+ns.orientation*v

        let inline timedShoot() =
            ns.elapsed_time <- ns.elapsed_time + 1
            if ns.elapsed_time > ns.cooldown then
                let k = Keyboard.GetState()
                if k.IsKeyDown(Keys.Space) then
                    ns.elapsed_time <- 0
                    sprite_manager_add ns.shootType ns.position

        let inline timedOrientedShoot() =
            ns.elapsed_time <- ns.elapsed_time + 1
            if ns.elapsed_time > ns.cooldown then
                let k = Keyboard.GetState()
                if k.IsKeyDown(Keys.Space) then
                    ns.elapsed_time <- 0
                    sprite_manager_add ns.shootType (ns.position + ns.orientation*(SPRITE_HEIGHT |> float32))

        let inline enemyShoot() =
            ns.elapsed_time <- ns.elapsed_time + 1
            if ns.elapsed_time > ns.cooldown && rng.NextDouble() < ns.probability then
                ns.elapsed_time <- 0
                sprite_manager_add ns.shootType ns.position

        let inline enemySpawn() =
            ns.elapsed_time <- ns.elapsed_time + 1
            if ns.total > 0 then
                if ns.elapsed_time > ns.cooldown then
                    ns.elapsed_time <- 0
                    ns.total <- ns.total-1
                    sprite_manager_add ns.shootType ns.position
            else killsprite_manager_add i // Add self to the termination queue when spawns left runs out.

        let inline flicker() =
            ns.elapsed_time <- ns.elapsed_time+1
            if ns.elapsed_time > ns.limit*10 then
                killsprite_manager_add i // Add self to the termination queue when spawns left runs out.


        match ns.mclass with
        | ShootAvatar ->
            avatarMoveEffect()
            timedOrientedShoot()
        | FlakAvatar -> 
            avatarMoveEffect()
            timedShoot()
        | Missile -> 
            passiveMoveEffect()
        | Bomber ->
            passiveMoveEffect()
            enemyShoot()
        | Spawnpoint ->
            enemySpawn()
        | Flicker ->
            flicker()
        | RandomNPC ->
            randomMove()
        | _ -> ()
        
        ns.toIm


    let binary_effect_manager_process =
        let width = graphics.PreferredBackBufferWidth
        let height = graphics.PreferredBackBufferHeight
        let l1, l2 = width/SPRITE_WIDTH, height/SPRITE_HEIGHT
        let buf = Array2D.init l1 l2 (fun _ _ -> ResizeArray<int>())
        if SPRITE_WIDTH <> 20 || SPRITE_HEIGHT <> 20 then failwith "Adjust this function!"

        /// Checks if the sprite is outside the window boundary.
        let inline is_sprite_outside_window px py =
            /// Checks if the sprite is outside the horizontal window boundary.
            let inline is_sprite_outside_window_horizontal px py =
                px < 0 || px + SPRITE_WIDTH > width
            /// Checks if the sprite is outside the vertical window boundary.
            let inline is_sprite_outside_window_vertical px py =
                py < 0 || py + SPRITE_HEIGHT > height

            is_sprite_outside_window_horizontal px py || is_sprite_outside_window_vertical px py

        fun () ->
            let add_sprites_to_buf() =
                buf |> Array2D.iter (fun x -> x.Clear())
                for i=0 to sprite_count-1 do
                    let x = sprites1.[i].rect
                    let px,py = x.X, x.Y
                    let px',py' = px/SPRITE_WIDTH,py/SPRITE_HEIGHT
                    
                    if px' < l1 && py' < l2 then buf.[px',py'].Add(i)
                    // Generalizing collision detection seems like way too much pain in the ass given 
                    // that I do not expect the sprites to be anything but 20x20.
                    // If the sprites were variable length, I would have to use a loop here.
                    if px'+1 < l1 && py' < l2 then buf.[px'+1,py'].Add(i) 
                    if px' < l1 && py'+1 < l2 then buf.[px',py'+1].Add(i)
                    if px'+1 < l1 && py'+1 < l2 then buf.[px'+1,py'+1].Add(i)

            let get_collisions_for i =
                let ri = sprites2.[i].rect
                let px, py = ri.X, ri.Y
                if is_sprite_outside_window px py = false then
                    let px', py' = px/SPRITE_WIDTH, py/SPRITE_HEIGHT
                    let rb = buf.[px',py']
                    [|
                    for x in rb do
                        if Rectangle.Intersect(ri,sprites2.[x].rect).Size <> Point(0) then
                            if x <> i then yield x
                    |]
                else [||]

            let inline get_eos_boundary_crossing_for i  =
                let ri = sprites2.[i].rect
                let px, py = ri.X, ri.Y
                is_sprite_outside_window px py

            let inline interaction_function messages i j =
                for x in messages do
                    let ra = sprites2.[i]
                    let rb = if j >= 0 then Some sprites2.[j] else None
                    match x with
                    | KillSprite -> 
                        killsprite_manager_add i
                    | ScoreChange x -> score <- score+x; score_string <- sprintf "Score: %i" score
                    | TurnAround | StepBack -> 
                        ra.queue.Add(x)
                    | CollectResource _ -> rb.Value.queue.Add(CollectResource ra.id) // TODO: is this really right. I forgot the logic of this.
                    | KillIfFromAbove x -> 
                        let sec = Rectangle.Intersect(ra.rect,rb.Value.rect_lower_half_horizontal)
                        if sec.Width-(STANDARD_SPEED |> int) > sec.Height then 
                            score <- score+x; score_string <- sprintf "Score: %i" score
                            killsprite_manager_add i
                    | TransformTo _ | KillIfOtherHasMore _ -> failwith "Should be tagged."
                    | KillIfOtherHasMoreTagged (resource,limit) -> 
                        match rb.Value.resources.TryFind(resource) with
                        | Some v -> if v >= limit then killsprite_manager_add i
                        | None -> if 0 >= limit then killsprite_manager_add i
                    | TransformToTagged x -> killsprite_manager_add i; sprite_manager_add x ra.position

            add_sprites_to_buf()
            for i=0 to sprite_count-1 do
                let cols = get_collisions_for i
                let idi = sprites1.[i].id
                if get_eos_boundary_crossing_for i then interaction_function tagged_eos.[idi] i -1
                for j in cols do
                    let idj = sprites1.[j].id
                    interaction_function tagged_neos.[idi,idj] i j
                    interaction_function tagged_neos.[idj,idi] j i

    let message_processor()=
        for i=0 to sprite_count-1 do
            let sprite_prev = sprites1.[i]
            let sprite = sprites2.[i]
            let mutable ns = VGDLMutableSprite.fromIm sprite

            for m in ns.queue do
                match m with
                | StepBack -> ns.position <- sprite_prev.position
                | TurnAround ->
                    ns.position.Y <- ns.position.Y + (SPRITE_HEIGHT |> float32)
                    ns.velocity <- Vector2(-ns.velocity.X,-ns.velocity.Y)
                | CollectResource id ->
                    match sprite.resources.TryFind(id) with
                    | Some v -> 
                        ns.resources <- ns.resources.Add(id, v+1)
                    | None -> 
                        ns.resources <- ns.resources.Add(id, 1)
                | _ -> ()

            ns.queue.Clear()

            sprites3.[i] <- ns.toIm

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
            for i=0 to sprite_count_new-1 do do
                let x = sprites3.[i]
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

        // Make a printer.
        printer <- VGDLSpriteFont((globalSpriteFontDict.Values |> Seq.toArray |> fun x -> x.[0]), Vector2(50.0f,50.0f))

        for i=0 to level_split.Length-1 do // Creates the level by initializing the spites.
            for j=0 to level_split.[i].Length-1 do
                let c = level_split.[i].[j]
                if c <> ' ' then
                    let sprite_id = c 
                                    |> level_mapping.TryFind 
                                    |> function Some x -> x 
                                              | None -> failwithf "No key for %c found at line %i, column %i!" (level_split.[i].[j]) (i+1) (j+1)

                    let x,y = j*SPRITE_WIDTH,i*SPRITE_HEIGHT
                    let position = Vector2(float32 x, float32 y)

                    sprites1.[sprite_count] <- initializer_map.Value.[sprite_id] position
                    sprite_count <- sprite_count+1
        
    override this.UnloadContent() =
        for x in globalTexure2dDict.Values do x.Dispose()
        globalTexure2dDict.Clear()
        globalSpriteFontDict.Clear()

    override this.Update(gameTime) = 
        if GamePad.GetState(PlayerIndex.One).Buttons.Back = ButtonState.Pressed || Keyboard.GetState().IsKeyDown(Keys.Escape)
        then this.Exit()

        sprite_count_new <- sprite_count
        for i=0 to sprite_count-1 do 
            sprites2.[i] <- unary_transition i
        binary_effect_manager_process()
        message_processor() // Processes the sprite messages
        sprite_manager_process() // Manages additions
        killsprite_manager_process() // Manages deletions

        // Terminators
        terminators_manager_process()
        |> fun x -> 
            match x with
            | Continue -> ()
            | _ -> 
                outcome_ref := score, x
                printfn "The score is: %i. Outcome: %A." score x
                this.Exit()

        swap_sprites()

        base.Update(gameTime)

    override this.Draw(gameTime) = 
        this.GraphicsDevice.Clear(Color.CornflowerBlue)

        spriteBatch.Begin()

        for i=0 to sprite_count-1 do sprites1.[i].draw spriteBatch
        printer.draw spriteBatch score_string

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
w.oo.....ooo..o......A...w
w.x..wow...x.. ...oo...www
w.x..wxw...... ...xx...oow
w.x..w.w b ... .wwwwww.xxw
w.x..w.w...oo. ........xxw
w.......xx.xx. ....oww...w
w.ooo..www.... ....x.....w
w.xxx.. b.....ww...x c ..w
w..w...  .....xx...x.....w
w..w.....wwwoo...........w
w....xx.....xx..c...E....w
wwwwwwwwwwwwwwwwwwwwwwwwww"""

let outcome = ref (0,Continue)
let g = new VGDLGame(aliens_spec,aliens_text,outcome)
g.Run()
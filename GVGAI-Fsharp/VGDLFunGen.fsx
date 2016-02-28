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
let GRID_WIDTH, GRID_HEIGHT = 10.f, 10.f

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
    member inline sprite.rect_upper_half_horizontal = 
        let position = sprite.grid
        Rectangle(int sprite.position.X, int sprite.position.Y, SPRITE_WIDTH, SPRITE_HEIGHT-10) // For horizontal cut intersections.
    member inline sprite.draw (spriteBatch: SpriteBatch) = 
        match sprite.texture with
        | Some texture -> 
            spriteBatch.Draw(texture, sprite.grid, Color.White)
        | None -> ()

type VGDLMutableSprite with
    member inline sprite.grid = Vector2((truncate <| sprite.position.X / GRID_WIDTH) * GRID_WIDTH, (truncate <| sprite.position.Y / GRID_HEIGHT) * GRID_HEIGHT)
    member inline sprite.rect = 
        let position = sprite.grid
        Rectangle(int position.X, int position.Y, SPRITE_WIDTH, SPRITE_HEIGHT)
    member inline sprite.rect_lower_half_horizontal = 
        let position = sprite.grid
        Rectangle(int sprite.position.X, int sprite.position.Y+10, SPRITE_WIDTH, SPRITE_HEIGHT-10) // For horizontal cut intersections.
    member inline sprite.rect_upper_half_horizontal = 
        let position = sprite.grid
        Rectangle(int sprite.position.X, int sprite.position.Y, SPRITE_WIDTH, SPRITE_HEIGHT-10) // For horizontal cut intersections.
    member inline sprite.draw (spriteBatch: SpriteBatch) = 
        match sprite.texture with
        | Some texture -> 
            spriteBatch.Draw(texture, sprite.grid, Color.White)
        | None -> ()

type VGDLSpriteFont =
    struct
    val font : SpriteFont
    val font_position : Vector2
    new(font,font_position) = {font=font;font_position=font_position}
    end

    member inline sprite.draw (spriteBatch: SpriteBatch) (score: string) = 
        spriteBatch.DrawString(sprite.font,score,sprite.font_position,Color.AliceBlue)

let inline binary_search(ar:StateType[]) fpos lpos (value : int) selector = // Type annotations for speed.
    let rec findRec fpos lpos =
        if fpos > lpos then
            -1
        else
            let mid = (fpos + lpos) / 2
            let value' = selector ar.[mid]
            if value < value' then
                findRec fpos (mid-1)
            else if value > value' then
                findRec (mid+1) lpos
            else
                mid
    findRec fpos lpos

/// The MonoGame engine class. Pass the VGDL game description and level as constructor inputs.
type VGDLGame(gameDesc: string, levelDesc: string, outcome_ref) as this =
    inherit Game()

    let id_map, tree_map, level_mapping, tagged_eos, tagged_neos, termination_set, reverse_hierarchy, initializer_map_create =
        runSematic gameDesc

    do for x in id_map do printfn "%A" x
    do printfn "%A" reverse_hierarchy

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

    // ...No, I cannot mutate the members of a ResizeArray unfortunately. Have to go back to plain arrays.
    let sprites = Array.init 10000 (fun _ -> StateType.def)
    let mutable sprite_index = 0 // The index to the sprites array that points to one after the last element.

    let sprite_tracker = Array.init tree_map.Count (fun _ -> ResizeArray())
    let mutable sprite_tracker_flag = false // The dirty flag for the sprite tracker to keep track whether it needs rebuilding.

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
                            |> ResizeArray.filter (fun (id,_) -> 
                                if binary_search sprites 0 (sprite_index-1) id (fun x -> x.sprite2.id) < 0 then true else false
                                ) // Filters out singletons present in the sprites array.
                            |> ResizeArray.iter (fun (id,pos) -> 
                                sprites.[sprite_index].sprite3 <- initializer_map.Value.[id] pos
                                sprite_tracker_flag <- true // sprite_tracker needs updating
                                sprite_index <- sprite_index+1)
                    
                    nons 
                    |> ResizeArray.iter (fun (id,pos) -> 
                        sprites.[sprite_index].sprite3 <- initializer_map.Value.[id] pos
                        sprite_tracker_flag <- true // sprite_tracker needs updating
                        sprite_index <- sprite_index+1
                        ) // Non singleton sprites are just added without limit.
                
                queue.Clear()
            
        add, process_queue


    let killsprite_manager_add, killsprite_manager_process =
        let queue = ResizeArray()
        let killsprite_manager_add i = queue.Add i
        let killsprite_manager_process () = // Queue based removal at indices. Identical to a hypothetical sprites.RemoveIndices(queue)
            if queue.Count > 0 then 
                queue.Sort()
                for i=queue.Count-2 downto 0 do
                    let m = queue.[i]
                    let n = queue.[i+1]
                    if m <> n then
                        sprite_index <- sprite_index-1
                        sprites.[n].sprite3 <- sprites.[sprite_index].sprite3
                        sprite_tracker_flag <- true // sprite_tracker needs updating
                
                let n = queue.[0]
                sprite_index <- sprite_index-1
                sprites.[n].sprite3 <- sprites.[sprite_index].sprite3
                sprite_tracker_flag <- true // sprite_tracker needs updating
                queue.Clear()

        killsprite_manager_add, killsprite_manager_process

    let unary_transition i =
        let mutable ns = VGDLMutableSprite.fromIm sprites.[i].sprite1

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
                if ns.elapsed_time > ns.cooldown && rng.NextDouble() < ns.probability then
                    ns.elapsed_time <- 0
                    ns.total <- ns.total-1
                    sprite_manager_add ns.shootType ns.position
            else killsprite_manager_add i // Add self to the termination queue when spawns left runs out.

        let inline flicker() =
            ns.elapsed_time <- ns.elapsed_time+1
            if ns.elapsed_time > ns.limit*10 then
                killsprite_manager_add i // Add self to the termination queue when spawns left runs out.

        let inline chase_or_flee is_chase =
            let closest_pos = // Returns the position of the closest stype sprite with the X and Y sum normalized to 1.
                let t = sprite_tracker.[ns.shootType] 
                let mutable l = Single.MaxValue
                let mutable lp = Vector2(0.0f,0.0f)
                for x in t do
                    let xp = ns.position - sprites.[x].sprite1.position
                    let xl = xp.LengthSquared()
                    if l > xl then
                        l <- xl
                        lp <- xp
                lp

            let ori =
                let hor =
                    let x = closest_pos.X / GRID_WIDTH |> truncate
                    if x < 0.0f then LEFT
                    else if x > 0.0f then RIGHT
                    else if rng.NextDouble() > 0.5 then LEFT else RIGHT
                let ver =
                    let y = closest_pos.Y / GRID_HEIGHT |> truncate
                    if y < 0.0f then UP
                    else if y > 0.0f then DOWN
                    else if rng.NextDouble() > 0.5 then UP else DOWN
                if rng.NextDouble() > 0.5 then hor*2.0f else ver*2.0f
            ns.position <- ns.position-ori*is_chase*ns.velocity
                
        match ns.mclass with
        | MovingAvatar ->
            avatarMoveEffect()
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
        | Chaser ->
            chase_or_flee 1.0f
        | Fleeing ->
            chase_or_flee -1.0f
        | Door | Resource | Immovable | Portal | NoClass -> ()
        
        ns.toIm


    let binary_effect_manager_process =
        let width = graphics.PreferredBackBufferWidth
        let height = graphics.PreferredBackBufferHeight
        let l1, l2 = width/SPRITE_WIDTH, height/SPRITE_HEIGHT
        let buf = Array2D.init l1 l2 (fun _ _ -> ResizeArray<int>())
        if SPRITE_WIDTH <> 20 || SPRITE_HEIGHT <> 20 then failwith "Adjust this function!"

        let messages = ResizeArray() // An optimization so these are not reinstantiated constantly.
        let pull_with_it_ar = ResizeArray() // I need this one to eliminate multiple same direction vectors in order to prevent the player from going too fast in a direction.

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
                for i=0 to sprite_index-1 do
                    let x = sprites.[i].sprite2.rect
                    let px,py = x.X, x.Y
                    let px',py' = px/SPRITE_WIDTH,py/SPRITE_HEIGHT
                    
                    for j= -1 to 1 do
                        for k= -1 to 1 do
                            let px' = px'+j
                            let py' = py'+k // Shadowing
                            if px' >= 0 && px' < l1 && py' >= 0 && py' < l2 then buf.[px',py'].Add(i)

            let get_collisions_for i =
                let ri = sprites.[i].sprite2.rect
                let ri' = sprites.[i].sprite1.rect
                let px, py = ri.X, ri.Y
                if is_sprite_outside_window px py = false then
                    let px', py' = px/SPRITE_WIDTH, py/SPRITE_HEIGHT
                    let rb = buf.[px',py']
                    [| // TODO: Optimize these computational expressions away by replacing them with preallocated ResizeArrays similar to the message one.
                    for x in rb do
                        if x <> i && 
                           (let t = Point(0)
                           Rectangle.Intersect(ri,sprites.[x].sprite1.rect).Size <> t) then
                            yield x
                    |],
                    [|
                    for x in rb do
                        if x <> i && 
                           (let t = Point(0)
                           Rectangle.Intersect(ri,sprites.[x].sprite2.rect).Size <> t ||
                           Rectangle.Intersect(ri,sprites.[x].sprite1.rect).Size <> t ||
                           Rectangle.Intersect(ri',sprites.[x].sprite2.rect).Size <> t ||
                           Rectangle.Intersect(ri',sprites.[x].sprite1.rect).Size <> t) then
                            yield x
                    |]
                else [||],[||]

            let inline get_eos_boundary_crossing_for i  =
                let ri = sprites.[i].sprite2.rect
                let px, py = ri.X, ri.Y
                is_sprite_outside_window px py


            let interaction_function i = // messages_del and messages_im are insantiated at the beginning of binary_effect_manager_process
                let mutable ns = VGDLMutableSprite.fromIm sprites.[i].sprite2
                let ra' = sprites.[i].sprite1

                for (x,o,j) in messages do
                    match x with
                    | StepBackTagged ->
                        ns.position <- ra'.position
                        pull_with_it_ar.Clear()
                    | TurnAroundTagged ->
                        ns.position.Y <- ns.position.Y + (SPRITE_HEIGHT |> float32)
                        ns.velocity <- -ns.velocity
                    | CollectResourceTagged resource -> // TODO: Do not forget to reverse this one.
                        match ns.resources.TryFind(resource) with
                        | Some v -> ns.resources <- ns.resources.Add(resource, v+1)
                        | None -> ns.resources <- ns.resources.Add(resource, 1)
                    | WrapAroundTagged ->
                        ns.position <- Vector2((ns.position.X + BACKBUFFER_WIDTH_F) % BACKBUFFER_WIDTH_F, (ns.position.Y + BACKBUFFER_HEIGHT_F) % BACKBUFFER_HEIGHT_F)
                    | PullWithItTagged ->
                        pull_with_it_ar.Add(sprites.[j].sprite2.position-sprites.[j].sprite1.position)
                    | KillSpriteTagged scoreChange -> 
                        score <- score+scoreChange; score_string <- sprintf "Score: %i" score
                        killsprite_manager_add i
                    | TransformToTagged(stype,scoreChange) -> 
                        score <- score+scoreChange; score_string <- sprintf "Score: %i" score
                        killsprite_manager_add i; sprite_manager_add stype ra'.position
                    | KillIfFromAboveTagged scoreChange -> 
                        let rb = sprites.[j].sprite2
                        let sec = Rectangle.Intersect(ns.rect,rb.rect_lower_half_horizontal)
                        if sec.Width-(STANDARD_SPEED |> int) > sec.Height && 
                            Rectangle.Intersect(ns.rect,rb.rect_upper_half_horizontal).Size = Point(0) then 
                            score <- score+scoreChange; score_string <- sprintf "Score: %i" score
                            killsprite_manager_add i
                    | KillIfHasLessTagged(resource,limit) ->
                        match ns.resources.TryFind(resource) with
                        | Some v -> if v <= limit then killsprite_manager_add i
                        | None -> if 0 <= limit then killsprite_manager_add i
                    | KillIfOtherHasMoreTagged (resource,limit) -> 
                        let rb = sprites.[j].sprite2
                        match rb.resources.TryFind(resource) with
                        | Some v -> if v >= limit then killsprite_manager_add i
                        | None -> if 0 >= limit then killsprite_manager_add i
                    | CloneSpriteTagged ->
                        sprite_manager_add ra'.id ra'.position
                    | ChangeResourceTagged(resource, value) ->
                        ns.resources <- ns.resources.Add(resource, value)
                    | ReverseDirectionTagged ->
                        ns.velocity <- -ns.velocity
                    | TeleportToExitTagged ->
                        ns.position <- 
                            let id = sprites.[j].sprite2.shootType
                            let pos = 
                                let t = binary_search sprites 0 (sprite_index-1) id (fun x -> x.sprite2.id)
                                if t < 0 then failwith "Cannot find an exit in TeleportToExit!"
                                t
                            let rec id_collect accum i dir =
                                if i >= 0 && i < sprite_index then 
                                    if id = sprites.[i].sprite2.id then id_collect (pos::accum) (i+dir) dir
                                    else accum
                                else accum
                            
                            List.append (id_collect [] pos 1) (id_collect [] (pos-1) -1)
                            |> fun x -> x.[rng.Next(0,x.Length)]
                            |> fun x -> sprites.[x].sprite2.position


                if pull_with_it_ar.Count > 0 then
                    ns.position <- ns.position + pull_with_it_ar.[rng.Next(0, pull_with_it_ar.Count)]
                    pull_with_it_ar.Clear()
                sprites.[i].sprite3 <- ns.toIm

            add_sprites_to_buf()
            for i=0 to sprite_index-1 do
                let cols_del, cols_im = get_collisions_for i
                let idi = sprites.[i].sprite1.id
                messages.Clear()
                if get_eos_boundary_crossing_for i then 
                    let f,s = tagged_eos
                    for (l,r) in f.[idi] do messages.Add((l,r,-1))
                    for (l,r) in s.[idi] do messages.Add((l,r,-1))
                for j in cols_im do
                    let idj = sprites.[j].sprite1.id
                    let f,s = tagged_neos
                    for (l,r) in f.[idi,idj] do messages.Add((l,r,j))
                for j in cols_del do
                    let idj = sprites.[j].sprite1.id
                    let f,s = tagged_neos
                    for (l,r) in s.[idi,idj] do messages.Add((l,r,j))

                messages |> ResizeArray.sortBy (fun (_,x,_) -> x)
                interaction_function i

    let terminators_manager_process =
        let get_counts() =
            let c = Array.zeroCreate tree_map.Count
            for i=0 to sprite_index-1 do do
                for x in reverse_hierarchy.[sprites.[i].sprite3.id] do
                    c.[x] <- c.[x]+1
            c

        fun () ->
            let c = get_counts()
            let rec loop i =
                if i < termination_set.Length then
                    match termination_set.[i] with
                    | SpriteCounterTagged(id,limit,win_or_loss) ->
                        if c.[id] = limit then 
                            win_or_loss
                        else loop (i+1)
                    | MultiSpriteCounterTagged(ids,limit,win_or_loss) ->
                        let rec test i =
                            if i < ids.Length then
                                if c.[ids.[i]] = limit then test (i+1)
                                else false
                            else true
                        if test 0 then win_or_loss
                        else loop (i+1)
                    | TimeoutTagged _ -> failwith "Timeout not implemented!"
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

                    sprite_id
                    |> Array.iter (fun sprite_id -> sprite_manager_add sprite_id position)
        
    override this.UnloadContent() =
        for x in globalTexure2dDict.Values do x.Dispose()
        globalTexure2dDict.Clear()
        globalSpriteFontDict.Clear()

    override this.Update(gameTime) = 
        if GamePad.GetState(PlayerIndex.One).Buttons.Back = ButtonState.Pressed || Keyboard.GetState().IsKeyDown(Keys.Escape)
        then this.Exit()

        for i=0 to sprite_index-1 do 
            sprites.[i].sprite2 <- unary_transition i
        binary_effect_manager_process() // The interaction set effects
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

        for i=0 to sprite_index-1 do 
            sprites.[i].sprite1 <- sprites.[i].sprite3

        if sprite_tracker_flag then
            Array.Sort(sprites,0,sprite_index,{new IComparer<StateType> with member t.Compare(x,y) = compare x.sprite1.id y.sprite1.id})

            for x in sprite_tracker do x.Clear()

            for i=0 to sprite_index-1 do
                let x = sprites.[i].sprite1.id
                sprite_tracker.[x].Add(i)

            sprite_tracker_flag <- false

        base.Update(gameTime)

    override this.Draw(gameTime) = 
        this.GraphicsDevice.Clear(Color.CornflowerBlue)

        spriteBatch.Begin()

        for i=0 to sprite_index-1 do sprites.[i].sprite1.draw spriteBatch
        printer.draw spriteBatch score_string

        spriteBatch.End()
        base.Draw(gameTime)

let aliens_spec = """BasicGame
    SpriteSet
        bullet > color=RED img=bullet
            sitting  > Immovable
            random   > RandomNPC speed=0.25
            straight > Missile   speed=0.5 img=missile
                vertical   > orientation=UP
                horizontal > orientation=LEFT
        structure > Immovable
            goal  > color=GREEN img=goal
            portalentry > Portal img=portal
                entry1 > stype=exit1 color=LIGHTBLUE
                entry2 > stype=exit2 color=BLUE
            portalexit  > color=BROWN img=door
                exit1  >
                exit2  >
    InteractionSet
        random wall      > stepBack
        random structure > stepBack
        avatar wall      > stepBack
        goal   avatar    > killSprite scoreChange=1
        avatar bullet    > killSprite
        straight wall    > reverseDirection
        avatar portalentry > teleportToExit

    TerminationSet
        SpriteCounter stype=goal   limit=0 win=True
        SpriteCounter stype=avatar limit=0 win=False

    LevelMapping
        h > horizontal
        v > vertical
        x > sitting
        r > random
        G > goal
        i > entry1
        I > entry2
        o > exit1
        O > exit2"""

let aliens_text = """wwwwwwwwwwwwwwwwwww
w  h    r   wI    w
w  o    wwww  w w w
wwwww  v         vw
w  ow   v w       w
wA iw     w  h v  w
wwwww    h     r  w
w           o     w
w  i w ww   wwwwwww
w   h v     wO   Gw
wwwwwwwwwwwwwwwwwww"""

let outcome = ref (0,Continue)
let g = new VGDLGame(aliens_spec,aliens_text,outcome)
g.Run()
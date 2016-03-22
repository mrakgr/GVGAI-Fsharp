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

let noise = 0.00 // Noise can be used to ameliorate round-off errors in enemy movement. Set it to 0.01.

let sprite_list =
    Directory.GetFiles (__SOURCE_DIRECTORY__ + @"\Sprites")
    |> Array.mapi (fun i x -> ((x.Split [|'\\'|] |> Array.last).Split [|'.'|]).[0],i)

let font_list =
    Directory.GetFiles (__SOURCE_DIRECTORY__ + @"\Fonts")
    |> Array.mapi (fun i x -> ((x.Split [|'\\'|] |> Array.last).Split [|'.'|]).[0],i)

let GRID_WIDTH, GRID_HEIGHT = 5.f, 5.f // Must be 1,2,5,10 or 20. Bad things will happen otherwise

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

/// The MonoGame engine class. Pass the VGDL game description and level as constructor inputs.
type VGDLGame(gameDesc: string, levelDesc: string, outcome_ref) as this =
    inherit Game()

    do this.TargetElapsedTime <- TimeSpan.FromSeconds(1.0 / 30.0) // Caps it to x FPS

    let  id_map, tree_map, level_mapping, tagged_eos, tagged_neos, termination_set, reverse_hierarchy, 
         initializer_map_create, resource_limits, resource_list, reverse_id_map, 
         reverse_singleton_hierarchy, avatar_id, stepbacks_neos =
        runSematic gameDesc

    do for x in id_map do printfn "%A" x
    do printfn "%A" reverse_hierarchy
    do tree_map |> Map.iter (fun k (v,_) -> printfn "%s, %A" (reverse_id_map.[k]) v)

    let mutable tick = 0
    let mutable score = 0

    let level_split = levelDesc.Split '\n' |> Array.map (fun x -> x.TrimEnd())

    let globalTexure2dDict = Dictionary(HashIdentity.Structural)
    let globalSpriteFontDict = Dictionary(HashIdentity.Structural)

    let initializer_map = lazy initializer_map_create globalTexure2dDict this

    // Create a new SpriteBatch, which can be used to draw textures.
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch> 

    do this.Content.RootDirectory <- __SOURCE_DIRECTORY__

    let graphics = new GraphicsDeviceManager(this)
    do graphics.PreferredBackBufferWidth <- level_split |> Array.map (fun x -> x.Length*SPRITE_WIDTH) |> Array.max
    do graphics.PreferredBackBufferHeight <- level_split.Length*SPRITE_HEIGHT // The program depends on sprites being 20x20 in dimension.

    let BACKBUFFER_WIDTH_F = graphics.PreferredBackBufferWidth |> float32
    let BACKBUFFER_HEIGHT_F = graphics.PreferredBackBufferHeight |> float32

    do this.IsMouseVisible <- true

    // As a design choice, the entire state of the game resides in these quadruple buffered arrays.
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
        let change_score v = score <- score+v

        let mutable queue = ResizeArray()
        let singleton_sprites = 
            tree_map 
            |> Map.filter (fun k v -> (fst v).singleton)
            |> Map.toArray 
            |> Array.map (fun (k,(v,_)) -> k)
            |> fun x -> HashSet(x,HashIdentity.Structural)
        let add id (pos: Vector2) direction scoreChange = queue.Add (id,pos,direction, scoreChange)
        let process_queue () = 
            if queue.Count > 0 then
                
                queue // TODO: All of this is no good. I have to use proper insertion and removal in order to get rid of sorting.
                |> ResizeArray.partition(fun (x,_,_,_) -> singleton_sprites.Contains x) // Paritions the queue into singleton and nonsingleton sprites.
                |> fun (s,nons) -> 
                    if s.Count > 0 then
                        s   |> ResizeArray.distinctBy (fun (x,_,_,_) -> x) // Pares down the singletons if more than one is present in the array.
                            |> ResizeArray.filter (fun (id,_,_,_) -> 
                                match reverse_singleton_hierarchy.[id] with
                                | Some v -> sprite_tracker.[v].Count = 0
                                | None -> failwith "This one should not trigger."
                                ) // Filters out singletons present in the sprites array.
                            |> fun x -> 
                                if x.Count > 0 then sprite_tracker_flag <- true // sprite_tracker needs updating
                                x
                            |> ResizeArray.iter (fun (id,pos,dir,scoreChange) -> 
                                sprites.[sprite_index].sprite4 <- initializer_map.Value.[id] pos dir
                                change_score scoreChange
                                sprite_index <- sprite_index+1)
                    
                    nons 
                    |> fun x -> 
                        if x.Count > 0 then sprite_tracker_flag <- true // sprite_tracker needs updating
                        x
                    |> ResizeArray.iter (fun (id,pos,dir,scoreChange) -> 
                        sprites.[sprite_index].sprite4 <- initializer_map.Value.[id] pos dir
                        change_score scoreChange
                        sprite_index <- sprite_index+1
                        ) // Non singleton sprites are just added without limit.
                queue.Clear()
            
        add, process_queue


    let killsprite_manager_add, killsprite_manager_process =
        let change_score v = score <- score+v

        let queue = ResizeArray()
        let killsprite_manager_add i scoreChange = queue.Add(i, scoreChange)
        let killsprite_manager_process () = // Queue based removal at indices. Identical to a hypothetical sprites.RemoveIndices(queue)
            if queue.Count > 0 then 
                sprite_tracker_flag <- true // sprite_tracker needs updating
                queue.Sort()
                
                let sprite_tracker_remove n =
                    let ids = reverse_hierarchy.[sprites.[n].sprite4.id]
                    for id in ids do
                        match sprite_tracker.[id].Remove(n) with
                        | true -> ()
                        | false -> failwith "sprite_tracker remove failed."
                
                for i=queue.Count-2 downto 0 do
                    let m,_ = queue.[i]
                    let n,s = queue.[i+1]
                    if m <> n then
                        sprite_index <- sprite_index-1
                        sprite_tracker_remove n
                        sprites.[n].sprite4 <- sprites.[sprite_index].sprite4
                        change_score s
                
                let n,s = queue.[0]
                sprite_index <- sprite_index-1
                sprite_tracker_remove n
                sprites.[n].sprite4 <- sprites.[sprite_index].sprite4
                change_score s
                queue.Clear()

        killsprite_manager_add, killsprite_manager_process

    let width = graphics.PreferredBackBufferWidth
    let height = graphics.PreferredBackBufferHeight

    /// Checks if the sprite is outside the window boundary.
    let is_sprite_outside_window px py =
        /// Checks if the sprite is outside the horizontal window boundary.
        let inline is_sprite_outside_window_horizontal px py =
            px < 0 || px >= width
        /// Checks if the sprite is outside the vertical window boundary.
        let inline is_sprite_outside_window_vertical px py =
            py < 0 || py >= height

        is_sprite_outside_window_horizontal px py || is_sprite_outside_window_vertical px py

    let unary_transition =
        /// Args: (goal_x,goal_y) * sprite_index -> path_size * path_list
        let get_path =
            let l1, l2 = width/SPRITE_WIDTH+1, height/SPRITE_HEIGHT+1
            let buf = Array2D.init l1 l2 (fun _ _ -> ResizeArray<int>())
            let mutable tick_of_last_added_to_buf = tick
            if SPRITE_WIDTH <> 20 || SPRITE_HEIGHT <> 20 then failwith "Adjust this function!"

            let add_sprites_to_buf() =
                if tick <> tick_of_last_added_to_buf then
                    tick_of_last_added_to_buf <- tick
                    buf |> Array2D.iter (fun x -> x.Clear())
                    for i=0 to sprite_index-1 do
                        let x = sprites.[i].sprite1.rect // This is the only difference from the function in binary manager.
                        let px,py = x.X, x.Y
                        let px',py' = px/SPRITE_WIDTH,py/SPRITE_HEIGHT
                    
                        for j= -1 to 1 do
                            for k= -1 to 1 do
                                let px' = px'+j
                                let py' = py'+k // Shadowing
                                if px' >= 0 && px' < l1 && py' >= 0 && py' < l2 then buf.[px',py'].Add(i)

            let is_stepback (px, py) i =
                is_sprite_outside_window px py = false
                &&
                    let px', py' = px/SPRITE_WIDTH, py/SPRITE_HEIGHT
                    let pr = Rectangle(px,py,SPRITE_WIDTH,SPRITE_HEIGHT)
                    buf.[px',py']
                    |> ResizeArray.exists(
                            fun x ->
                            x <> i &&
                            Rectangle.Intersect(sprites.[x].sprite1.rect,pr).Size <> Point(0) &&
                            stepbacks_neos.[sprites.[i].sprite1.id,sprites.[x].sprite1.id])
                    |> not

            let GRID_HEIGHT_INT = GRID_HEIGHT |> int
            let GRID_WIDTH_INT = GRID_WIDTH |> int

            let l1 = (width + 20) / GRID_WIDTH_INT 
            let l2 = (height + 20) / GRID_HEIGHT_INT
            let trace = Array2D.zeroCreate l1 l2 // I guess I can afford to use a little extra memory compared to a Dictionary.
            let mutable later = Stack<(int*int)*int*float>(20000)
            let mutable now = Stack<(int*int)*int*float>(20000)

            fun (food_x, food_y as food_pos) i' -> // fringe_search
                add_sprites_to_buf() // Evaluated only once per tick.

                let mutable max_goal = Int32.MaxValue
                let pac_x, pac_y as pac_pos = sprites.[i'].sprite1.grid |> fun x -> int x.X, int x.Y

                Array.Clear(trace,0,trace.Length)
                later.Clear()
                now.Clear()
            
                let inline set (x, y) i =
                    trace.[x / GRID_WIDTH_INT, y / GRID_HEIGHT_INT] <- i
                let inline is_not_visited (x, y) =
                    let x, y = x / GRID_WIDTH_INT, y / GRID_HEIGHT_INT
                    x >= 0 && y >= 0 && x < l1 && y < l2 && trace.[x, y] = 0

                let inline up (pac_x, pac_y as p) = (pac_x, pac_y-GRID_HEIGHT_INT)
                let inline down (pac_x, pac_y as p) = (pac_x, pac_y+GRID_HEIGHT_INT)
                let inline left (pac_x, pac_y as p) = (pac_x-GRID_WIDTH_INT, pac_y)
                let inline right (pac_x, pac_y as p) = (pac_x+GRID_WIDTH_INT, pac_y)

                let rec get_trace_from (pac_x, pac_y as p) i accum =
                    let mutable n = None
                    let inline is_visited_and_less_than_i (x, y) dir =
                        let x',y' = x / GRID_WIDTH_INT, y / GRID_HEIGHT_INT
                        x' >= 0 && y' >= 0 && x' < l1 && y' < l2 && trace.[x',y'] <> 0 && trace.[x',y'] = i-1 && (n <- Some (x,y,dir); true) // n is assigned in this last expression
                    (is_visited_and_less_than_i <| up p <| DOWN // UP
                    || is_visited_and_less_than_i <| left p <| RIGHT // LEFT
                    || is_visited_and_less_than_i <| right p <| LEFT // RIGHT
                    || is_visited_and_less_than_i <| down p <| UP) // DOWN
                    |> ignore
                    n
                    |> function
                       | Some (x,y,d) -> get_trace_from (x,y) (i-1) (d::accum)
                       | None -> if accum.IsEmpty then [Vector2(0.0f,0.0f)] else accum

                let inline manhattan_distance (x,y) dist_to_source =
                    let a = abs(x-food_x) / GRID_WIDTH_INT 
                    let b = abs(y-food_y) / GRID_HEIGHT_INT
                    let c = dist_to_source 
                    a+b+c |> float


                let mutable later_upper_bound = Double.MaxValue

                let rec fringe_search upper_bound =
                    if max_goal = Int32.MaxValue then
                        if now.Count > 0 then
                            let (pac_x,pac_y as p),i,heuristic_cost as current_item = now.Pop()
                            if heuristic_cost > upper_bound 
                            then 
                                if heuristic_cost < later_upper_bound then later_upper_bound <- heuristic_cost
                                later.Push current_item
                                fringe_search upper_bound
                            else
                                let print_trace (trace : int[,])=
                                    for i=0 to l2-1 do
                                        for j=0 to l1-1 do
                                            printf "%5i" trace.[j,i]
                                        printfn ""
            
                                

                                let inline if_viable_execute x =
                                    let inline is_viable (n_x, n_y as x) = is_stepback x i' && is_not_visited x
                                    let inline execute x =
                                        let next_i = i+1
                                        if x <> food_pos then
                                            let c = manhattan_distance x next_i
                                            if c <= upper_bound then 
                                                now.Push((x,next_i,c))
                                            else 
                                                if c < later_upper_bound then later_upper_bound <- c
                                                later.Push((x,next_i,c))

                                            set x next_i 
                                        else set x next_i; max_goal <- next_i

                                    if is_viable x then execute x

                                if_viable_execute <| up p // UP
                                if_viable_execute <| left p // LEFT
                                if_viable_execute <| right p // RIGHT
                                if_viable_execute <| down p // DOWN

                                //printfn "-----"
                                //print_trace trace
                        
                                fringe_search upper_bound
                        else
                            if later.Count <> 0 then 
                                let t = now
                                now <- later
                                later <- t
                                let t' = later_upper_bound
                                later_upper_bound <- Double.MaxValue
                                fringe_search t'
            
                now.Push((pac_pos,1,manhattan_distance pac_pos 1))
                set pac_pos 1
                fringe_search <| manhattan_distance pac_pos 1
                let t = get_trace_from food_pos max_goal []
                max_goal, t//, expanded_nodes

        fun i ->
            let mutable ns = VGDLMutableSprite.fromIm sprites.[i].sprite1

            let inline avatarMoveEffect() =
                let k = Keyboard.GetState()
                if k.IsKeyDown(Keys.Left) then 
                    ns.orientation <- LEFT*ns.speed
                    ns.position <- ns.position+ns.orientation
                else if k.IsKeyDown(Keys.Right) then 
                    ns.orientation <- RIGHT*ns.speed
                    ns.position <- ns.position+ns.orientation
                else if k.IsKeyDown(Keys.Down) then 
                    ns.orientation <- DOWN*ns.speed
                    ns.position <- ns.position+ns.orientation
                else if k.IsKeyDown(Keys.Up) then 
                    ns.orientation <- UP*ns.speed
                    ns.position <- ns.position+ns.orientation

            /// Moves the sprite in the direction of its velocity.
            let inline passiveMoveEffect() =
                let inline add_opt_noise x = // Noise can be used to ameliorate unneven enemy movement.
                    if noise <> 0.0 then x*(float32 <| noise*rng.NextDouble()) else 0.0f
                let vx = ns.orientation.X
                let vy = ns.orientation.Y
                ns.position <- Vector2(ns.position.X + vx + add_opt_noise vx,ns.position.Y + vy + add_opt_noise vy)

            let randomMove() =
                ns.duration <- ns.duration-1
                if ns.duration < 0 then
                    ns.orientation <- ORIENTATIONS.[rng.Next(4)]*ns.speed
                    ns.duration <- rng.Next(2,10)
                ns.position <- ns.position+ns.orientation

            let inline timedShoot() =
                ns.elapsed_time <- ns.elapsed_time + 1
                if ns.elapsed_time > ns.cooldown then
                    let k = Keyboard.GetState()
                    if k.IsKeyDown(Keys.Space) then
                        ns.elapsed_time <- 0
                        let ammo_id = ns.ammo
                        if ammo_id = -1 then
                            sprite_manager_add ns.shootType ns.position None 0
                        else
                            let ammo = defaultArg (ns.resources.TryFind ammo_id) 0
                            if  ammo >= ns.min_ammo && 
                                (match reverse_singleton_hierarchy.[ns.shootType] with
                                | Some v -> sprite_tracker.[v].Count = 0
                                | None -> true) 
                            then
                                ns.resources <- ns.resources.Add(ammo_id,ammo-ns.ammo_cost)
                                sprite_manager_add ns.shootType ns.position None 0

            let inline timedOrientedShoot() =
                ns.elapsed_time <- ns.elapsed_time + 1
                if ns.elapsed_time > ns.cooldown then
                    let k = Keyboard.GetState()
                    if k.IsKeyDown(Keys.Space) then
                        ns.elapsed_time <- 0
                        let next_p =
                            let inline sign x = if x > 0.0f then 1.0f else if x < 0.0f then -1.0f else 0.0f
                            let x = ns.position.X + (ns.orientation.X |> sign)*(SPRITE_WIDTH |> float32)
                            let y = ns.position.Y + (ns.orientation.Y |> sign)*(SPRITE_HEIGHT |> float32)
                            Vector2(x,y)
                        let ammo_id = ns.ammo
                        if ammo_id = -1 then
                            sprite_manager_add ns.shootType next_p (ns.orientation |> Some) 0
                        else
                            let ammo = defaultArg (ns.resources.TryFind ammo_id) 0
                            if  ammo >= ns.min_ammo && 
                                (match reverse_singleton_hierarchy.[ns.shootType] with
                                | Some v -> sprite_tracker.[v].Count = 0
                                | None -> true) 
                            then
                                ns.resources <- ns.resources.Add(ammo_id,ammo-ns.ammo_cost)
                                sprite_manager_add ns.shootType next_p (ns.orientation |> Some) 0

            let inline enemyShoot() =
                ns.elapsed_time <- ns.elapsed_time + 1
                if ns.elapsed_time > ns.cooldown && rng.NextDouble() < ns.probability then
                    ns.elapsed_time <- 0
                    sprite_manager_add ns.shootType ns.position None 0

            let inline enemySpawn() =
                ns.elapsed_time <- ns.elapsed_time + 1
                if ns.total > 0 then
                    if ns.elapsed_time > ns.cooldown && rng.NextDouble() < ns.probability then
                        ns.elapsed_time <- 0
                        ns.total <- ns.total-1
                        sprite_manager_add ns.shootType ns.position None 0
                else killsprite_manager_add i 0 // Add self to the termination queue when spawns left runs out.

            let inline flicker() =
                ns.elapsed_time <- ns.elapsed_time+1
                if ns.elapsed_time > ns.limit*10 then
                    killsprite_manager_add i 0 // Add self to the termination queue when spawns left runs out.

            let inline chase_or_flee is_chase =
                let closest_pos = 
                    let t = sprite_tracker.[ns.shootType] 
                    let mutable l = Single.MaxValue
                    let mutable lp = Vector2(0.0f,0.0f)
                    for x in t do
                        let xp = ns.grid - sprites.[x].sprite1.grid
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
                ns.position <- ns.position-ori*(is_chase*ns.speed)

            let inline alt_chase_flee() =
                let closest_pos_chase = 
                    let s1 = tree_map.[ns.id] |> fst |> fun x -> x.shootType1
                    let t = sprite_tracker.[s1] 
                    let mutable l = Single.MaxValue
                    let mutable lp = None

                    for x in t do
                        let xp = ns.grid - sprites.[x].sprite1.grid
                        let xl = xp.LengthSquared()
                        if l > xl then
                            l <- xl
                            lp <- Some xp
                    lp

                let closest_pos_flee = 
                    let s1 = tree_map.[ns.id] |> fst |> fun x -> x.shootType2
                    let t = sprite_tracker.[s1] 
                    let mutable l = Single.MaxValue
                    let mutable lp = None

                    for x in t do
                        let xp = ns.grid - sprites.[x].sprite1.grid
                        let xl = xp.LengthSquared()
                        if l > xl then
                            l <- xl
                            lp <- Some xp
                    lp

                let closest_pos, is_chase =
                    match closest_pos_chase, closest_pos_flee with
                    | Some x, Some y -> if x.LengthSquared() > y.LengthSquared() then x, 1.0f else y, -1.0f
                    | Some x, None -> x, 1.0f
                    | None, Some y -> y, -1.0f
                    | None, None -> Vector2(0.0f,0.0f), 0.0f

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
                ns.position <- ns.position-ori*(is_chase*ns.speed)

            let inline random_alt_chase_flee() =
                ns.duration <- ns.duration-1
                if ns.duration >= 0 then
                    ns.position <- ns.position+ns.orientation
                else
                    if rng.NextDouble() < (tree_map.[ns.id] |> fst |> fun x -> x.epsilon) then
                        ns.orientation <- ORIENTATIONS.[rng.Next(4)]*ns.speed
                        ns.duration <- rng.Next(2,10)
                    else alt_chase_flee()
                

            let inline spread() =
                for orientation in ORIENTATIONS do
                    if rng.NextDouble() < ns.probability then
                        let next_p =
                            let inline sign x = if x > 0.0f then 1.0f else if x < 0.0f then -1.0f else 0.0f
                            let x = ns.position.X + (orientation.X |> sign)*(SPRITE_WIDTH |> float32)
                            let y = ns.position.Y + (orientation.Y |> sign)*(SPRITE_HEIGHT |> float32)
                            Vector2(x,y)
                        sprite_manager_add ns.shootType next_p None 0
                killsprite_manager_add i 0

            let inline pathMove() =
                let closest_pos = 
                    let s1 = tree_map.[ns.id] |> fst |> fun x -> x.shootType1
                    let t = sprite_tracker.[s1] 
                    let mutable l = Single.MaxValue
                    let mutable lp = None
                    for x in t do
                        let xl = (ns.grid - sprites.[x].sprite1.grid).LengthSquared()
                        if l > xl then
                            l <- xl
                            lp <- Some sprites.[x].sprite1.grid // Careful, the chaser function calculates the vector to the closest one instead of the actual position.
                    lp

                match closest_pos with
                | Some closest_pos ->
                    let ori = get_path (int closest_pos.X, int closest_pos.Y) i |> snd |> List.head
                    ns.position <- ns.position+ori*ns.speed
                | None -> ()

            match ns.mclass with
            | MovingAvatar ->
                avatarMoveEffect()
            | ShootAvatar | OrientedAvatar ->
                timedOrientedShoot()
                avatarMoveEffect()
            | FlakAvatar -> 
                timedShoot()
                avatarMoveEffect()
            | Missile | RandomMissile -> 
                passiveMoveEffect()
            | Bomber ->
                enemyShoot()
                passiveMoveEffect()
            | Spawnpoint ->
                enemySpawn()
            | Flicker | OrientedFlicker ->
                flicker()
            | RandomNPC -> // TODO: Make the alt chaser work. I am not too sure what it is supposed to be doing now.
                randomMove()
            | PathChaser ->
                pathMove()
            | Chaser ->
                chase_or_flee 1.0f
            | Fleeing ->
                chase_or_flee -1.0f
            | RandomAltChaser ->
                random_alt_chase_flee()
            | Spreader ->
                spread()
            | Door | Resource | Immovable | Portal | NoClass | Passive -> ()
        
            ns.toIm


    let binary_effect_manager_process_primaries, binary_effect_manager_process_secondaries =
        let l1, l2 = width/SPRITE_WIDTH, height/SPRITE_HEIGHT
        let buf = Array2D.init l1 l2 (fun _ _ -> ResizeArray<int>())
        if SPRITE_WIDTH <> 20 || SPRITE_HEIGHT <> 20 then failwith "Adjust this function!"

        let messages = ResizeArray() // An optimization so these are not reinstantiated constantly.
        let messages_sec = ResizeArray()
        let pull_with_it_ar = ResizeArray() // I need this one to eliminate multiple same direction vectors in order to prevent the player from going too fast in a direction.

        let add_sprites_to_buf () =
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
            //let px, py = ri'.X, ri'.Y // TODO: There was a reason for using this one, but I can't remember what. Most likely not using this will cause a bug. Figure out what kind of bug.
            let px, py = ri.X, ri.Y
            if is_sprite_outside_window px py = false then
                let px', py' = px/SPRITE_WIDTH, py/SPRITE_HEIGHT
                let rb = buf.[px',py']
                let t = Point(0)
                [| // TODO: Optimize these computational expressions away by replacing them with preallocated ResizeArrays similar to the message one.
                for x in rb do
                    if x <> i && 
                        (Rectangle.Intersect(ri,sprites.[x].sprite1.rect).Size <> t) then
                        yield x
                |],
                [|
                for x in rb do
                    if x <> i && 
                        (Rectangle.Intersect(ri,sprites.[x].sprite2.rect).Size <> t ||
                         Rectangle.Intersect(ri,sprites.[x].sprite1.rect).Size <> t ||
                         Rectangle.Intersect(ri',sprites.[x].sprite2.rect).Size <> t ||
                         Rectangle.Intersect(ri',sprites.[x].sprite1.rect).Size <> t) then
                        yield x
                |]
            else [||],[||]

        let get_collisions_for_secondaries i = 
            // I really need pointers to structs badly. Too bad F# does not allow passing them by reference. 
            // Should be added in the future: https://github.com/dotnet/roslyn/issues/118 https://github.com/dotnet/roslyn/pull/4042
            // https://github.com/dotnet/roslyn/issues/5233
            let ri = sprites.[i].sprite3.rect
            let ri' = sprites.[i].sprite1.rect
            // let px, py = ri'.X, ri'.Y // TODO: Same warning as before.
            let px, py = ri.X, ri.Y
            if is_sprite_outside_window px py = false then
                let px', py' = px/SPRITE_WIDTH, py/SPRITE_HEIGHT
                let rb = buf.[px',py']
                [| // TODO: Optimize these computational expressions away by replacing them with preallocated ResizeArrays similar to the message one.
                for x in rb do
                    if x <> i && 
                        (let t = Point(0)
                        Rectangle.Intersect(ri,sprites.[x].sprite3.rect).Size <> t) then
                        yield x
                |]
            else [||]

        let inline get_eos_boundary_crossing_for i  =
            let ri = sprites.[i].sprite2.rect
            let px, py = ri.X, ri.Y
            is_sprite_outside_window px py

        let inline get_eos_boundary_crossing_for_secondary i  =
            let ri = sprites.[i].sprite3.rect
            let px, py = ri.X, ri.Y
            is_sprite_outside_window px py


        let interaction_function i = // messages_del and messages_im are insantiated at the beginning of binary_effect_manager_process
            
            let mutable ns = VGDLMutableSprite.fromIm sprites.[i].sprite2
            let ra' = sprites.[i].sprite1

            let add_resource resource value scoreChange =
                let maxr = resource_limits.[resource]
                match ns.resources.TryFind resource with
                | Some v -> ns.resources <- ns.resources.Add(resource, min maxr (value+v) |> fun v -> max 0 v)
                | None -> ns.resources <- ns.resources.Add(resource, min maxr value |> fun v -> max 0 v)
                score <- score+scoreChange

            let mutable tranformto_flag = true

            for (x,o,j) in messages do
                match x with
                | StepBackTagged ->
                    ns.position <- ra'.position
                | StepBackAndClearPullWithItTagged -> // If PullWithIt exists, this gets triggered instead of StepBackTagged.
                    ns.position <- ra'.position
                    pull_with_it_ar.Clear()
                | TurnAroundTagged ->
                    ns.position.Y <- ns.position.Y + (SPRITE_HEIGHT |> float32)
                    ns.orientation <- -ns.orientation
                | CollectResourceTagged(resource, value, scoreChange) ->
                    add_resource resource value scoreChange
                | WrapAroundTagged ->
                    ns.position <- Vector2((ns.position.X + BACKBUFFER_WIDTH_F) % BACKBUFFER_WIDTH_F, (ns.position.Y + BACKBUFFER_HEIGHT_F) % BACKBUFFER_HEIGHT_F)
                | PullWithItTagged ->
                    pull_with_it_ar.Add(sprites.[j].sprite2.grid-sprites.[j].sprite1.grid)
                | KillSpriteTagged scoreChange -> 
                    killsprite_manager_add i scoreChange
                | TransformToTagged(stype,scoreChange) -> 
                    if tranformto_flag then
                        let rb = sprites.[j].sprite2
                        killsprite_manager_add i scoreChange; sprite_manager_add stype ra'.position ( 
                            if tree_map.[stype] |> fst |> (fun x -> x.orientation = (0.0f,0.0f)) then
                                rb.orientation |> Some
                            else None) 0
                        tranformto_flag <- false
                    else tranformto_flag <- true
                | KillIfFromAboveTagged scoreChange -> 
                    let rb = sprites.[j].sprite2
                    let sec = Rectangle.Intersect(ns.rect,rb.rect_lower_half_horizontal)
                    if sec.Width-(STANDARD_SPEED |> int) > sec.Height && 
                        Rectangle.Intersect(ns.rect,rb.rect_upper_half_horizontal).Size = Point(0) then 
                        killsprite_manager_add i scoreChange
                | KillIfHasLessTagged(resource,limit,scoreChange) ->
                    match ns.resources.TryFind(resource) with
                    | Some v -> if v <= limit then killsprite_manager_add i scoreChange
                    | None -> if 0 <= limit then killsprite_manager_add i scoreChange
                | KillIfHasMoreTagged(resource,limit,scoreChange) ->
                    match ns.resources.TryFind(resource) with
                    | Some v -> if v >= limit then killsprite_manager_add i scoreChange
                    | None -> if 0 >= limit then killsprite_manager_add i scoreChange
                | KillIfOtherHasMoreTagged (resource,limit,scoreChange) -> 
                    let rb = sprites.[j].sprite2
                    match rb.resources.TryFind(resource) with
                    | Some v -> if v >= limit then killsprite_manager_add i scoreChange
                    | None -> if 0 >= limit then killsprite_manager_add i scoreChange
                | CloneSpriteTagged ->
                    sprite_manager_add ra'.id ra'.position (ra'.orientation |> Some) 0
                | ChangeResourceTagged(resource,value,scoreChange) ->
                    add_resource resource value scoreChange
                | ReverseDirectionTagged ->
                    ns.orientation <- -ns.orientation
                | TeleportToExitTagged ->
                    ns.position <- 
                        let id = sprites.[j].sprite2.shootType
                        sprite_tracker.[id]
                        |> fun x -> x.[rng.Next(0,x.Count)]
                        |> fun x -> sprites.[x].sprite2.position
                | BounceForwardTagged ->
                    ns.position <- ns.position + sprites.[j].sprite2.grid-sprites.[j].sprite1.grid
                | FlipDirectionTagged ->
                    ns.position <- ra'.position
                    ns.orientation <- ns.speed*ORIENTATIONS.[rng.Next(0,ORIENTATIONS.Length)]
                | SpawnIfHasMoreTagged(resource,stype,limit,scoreChange) ->
                    match ns.resources.TryFind(resource) with
                    | Some v -> if v >= limit then sprite_manager_add stype ns.position None scoreChange
                    | None -> if 0 >= limit then sprite_manager_add stype ns.position None scoreChange
                | SpawnIfHasLessTagged(resource,stype,limit,scoreChange) ->
                    match ns.resources.TryFind(resource) with
                    | Some v -> if v <= limit then sprite_manager_add stype ns.position None scoreChange
                    | None -> if 0 <= limit then sprite_manager_add stype ns.position None scoreChange

            if pull_with_it_ar.Count > 0 then
                ns.position <- ns.position + pull_with_it_ar.[rng.Next(0, pull_with_it_ar.Count)]
                pull_with_it_ar.Clear()
            sprites.[i].sprite3 <- ns.toIm

        let interaction_function_secondary i = // messages_del and messages_im are insantiated at the beginning of binary_effect_manager_process
            let mutable ns = VGDLMutableSprite.fromIm sprites.[i].sprite3
            let ra' = sprites.[i].sprite1

            for (x,j) in messages_sec do
                match x with
                | StepBackSecondary -> ns.position <- ra'.position

            sprites.[i].sprite4 <- ns.toIm

        let process_primaries() =
            add_sprites_to_buf()
            for i=0 to sprite_index-1 do
                let cols_del,cols_im = get_collisions_for i
                let idi = sprites.[i].sprite1.id
                messages.Clear()
                if get_eos_boundary_crossing_for i then 
                    let im,del,sec = tagged_eos
                    for (l,r) in im.[idi] do messages.Add((l,r,-1))
                    for (l,r) in del.[idi] do messages.Add((l,r,-1))
                for j in cols_im do
                    let idj = sprites.[j].sprite1.id
                    let im,del,sec = tagged_neos
                    for (l,r) in im.[idi,idj] do messages.Add((l,r,j))
                for j in cols_del do
                    let idj = sprites.[j].sprite1.id
                    let im,del,sec = tagged_neos
                    for (l,r) in del.[idi,idj] do messages.Add((l,r,j))

                messages |> ResizeArray.sortBy (fun (_,x,_) -> x)
                interaction_function i

        let process_secondaries() =
            for i=0 to sprite_index-1 do
                let cols_sec = get_collisions_for_secondaries i
                let idi = sprites.[i].sprite1.id
                messages_sec.Clear()
                if get_eos_boundary_crossing_for_secondary i then 
                    let im,del,sec = tagged_eos
                    for l in sec.[idi] do messages_sec.Add((l,-1))
                for j in cols_sec do
                    let idj = sprites.[j].sprite1.id
                    let im,del,sec = tagged_neos
                    for l in sec.[idi,idj] do messages_sec.Add((l,j))

                interaction_function_secondary i

        process_primaries, process_secondaries

    let terminators_manager_process =
        fun () ->
            let rec loop i =
                if i < termination_set.Length then
                    match termination_set.[i] with
                    | SpriteCounterTagged(id,limit,win_or_loss) ->
                        if sprite_tracker.[id].Count = limit then 
                            win_or_loss
                        else loop (i+1)
                    | MultiSpriteCounterTagged(ids,limit,win_or_loss) ->
                        let rec test i =
                            if i < ids.Length then
                                if sprite_tracker.[ids.[i]].Count = limit then test (i+1)
                                else false
                            else true
                        if test 0 then win_or_loss
                        else loop (i+1)
                    | TimeoutTagged(limit, win_or_loss) -> 
                        if tick >= limit then win_or_loss else loop (i+1)
                else Continue
            loop 0


    override this.Initialize() = base.Initialize()

    override this.LoadContent() = 
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)
        for x,i in sprite_list do // Add textures to the global dict.
            globalTexure2dDict.Add(x.ToLower(),this.Content.Load<Texture2D>(@"Sprites\"+x))
        for x,i in font_list do // Add sprite fonts to the global dict.
            globalSpriteFontDict.Add(x.ToLower(),this.Content.Load<SpriteFont>(@"Fonts\"+x))

        // Make a printer.
        printer <- VGDLSpriteFont((globalSpriteFontDict.Values |> Seq.toArray |> fun x -> x.[0]), Vector2(50.0f,50.0f))

        for i=0 to level_split.Length-1 do // Creates the level by initializing the spites.
            for j=0 to level_split.[i].Length-1 do
                let c = level_split.[i].[j]
                if c <> ' ' then
                    c 
                    |> level_mapping.TryFind 
                    |> function 
                        | Some sprite_id -> 
                            let x,y = j*SPRITE_WIDTH,i*SPRITE_HEIGHT
                            let position = Vector2(float32 x, float32 y)

                            sprite_id
                            |> Array.iter (fun sprite_id -> sprite_manager_add sprite_id position None 0)
                        | None -> printfn "No key for %c found at line %i, column %i! Skipping character..." (level_split.[i].[j]) (i+1) (j+1)
        
    override this.UnloadContent() =
        for x in globalTexure2dDict.Values do x.Dispose()
        globalTexure2dDict.Clear()
        globalSpriteFontDict.Clear()

    override this.Update(gameTime) = 
        if GamePad.GetState(PlayerIndex.One).Buttons.Back = ButtonState.Pressed || Keyboard.GetState().IsKeyDown(Keys.Escape)
        then this.Exit()

        for i=0 to sprite_index-1 do 
            sprites.[i].sprite2 <- unary_transition i
        binary_effect_manager_process_primaries() // The interaction set effects
        binary_effect_manager_process_secondaries() // Specially made for the UndoAll effect

        killsprite_manager_process() // Manages deletions
        sprite_manager_process() // Manages additions

        for i=0 to sprite_index-1 do 
            sprites.[i].sprite1 <- sprites.[i].sprite4

        if sprite_tracker_flag then
            // TODO: Remove sorting by implementing proper removal and insertion functions.
            Array.Sort(sprites,0,sprite_index,{new IComparer<StateType> with member t.Compare(x,y) = compare x.sprite1.id y.sprite1.id})

            for x in sprite_tracker do x.Clear()

            for i=0 to sprite_index-1 do
                let id = sprites.[i].sprite1.id
                for x in reverse_hierarchy.[id] do
                    sprite_tracker.[x].Add(i)

            sprite_tracker_flag <- false

        // Terminators
        terminators_manager_process()
        |> fun x -> 
            match x with
            | Continue -> ()
            | _ -> 
                outcome_ref := score, x
                printfn "The score is: %i. Outcome: %A." score x
                this.Exit()

        tick <- tick+1

        base.Update(gameTime)

    override this.Draw(gameTime) = 
        this.GraphicsDevice.Clear(Color.CornflowerBlue)

        spriteBatch.Begin()

        for i=0 to sprite_index-1 do sprites.[i].sprite1.draw spriteBatch

        let printer_string =                 
            let sb = System.Text.StringBuilder() // String builder is mutable.
            sb.AppendFormat("Tick: {0}, Score: {1}", tick/TICK_ADJUSTER, score) |> ignore
            if resource_list.Length > 0 then sb.Append(", ") |> ignore
            for (k,v) in resource_list do
                let k = Globalization.CultureInfo.CurrentCulture.TextInfo.ToTitleCase(k)
                if sprite_tracker.[avatar_id].Count > 0 then
                    match sprites.[sprite_tracker.[avatar_id].[0]].sprite4.resources.TryFind v with
                    | Some q -> sb.AppendFormat("{0}: {1}, ", k, q) |> ignore
                    | None -> sb.AppendFormat("{0}: {1}, ", k, 0) |> ignore
            sb.ToString()
        
        printer.draw spriteBatch printer_string

        spriteBatch.End()
        base.Draw(gameTime)

let aliens_spec = """BasicGame square_size=32
    SpriteSet
        food > Immovable
            fruit > color=PINK img=mushroom
            pellet > color=LIGHTYELLOW shrinkfactor=0.5 img=pellet
            power  > Resource color=LIGHTGREEN shrinkfactor=0.8 img=powerpill
        nest > SpawnPoint img=portal portal=True
            redspawn > stype=red
            orangespawn > stype=orange
            bluespawn > stype=blue
            pinkspawn > stype=pink
        moving >
            ghost > RandomAltChaser stype1=hungry stype2=powered cooldown=3 epsilon=0.25 img=ghost
                red    > color=LIGHTRED    singleton=True
                blue   > color=LIGHTBLUE   singleton=True
                pink   > color=PINK        singleton=True
                orange > color=LIGHTORANGE singleton=True
            pacman > OrientedAvatar rotateInPlace=false
                hungry  > color=YELLOW img=avatarYellow
                powered > color=ORANGE img=avatarRed
            
    InteractionSet
        moving wall > stepBack
        pacman EOS  > wrapAround
        ghost EOS  > stepBack
        power hungry  > killSprite
        ghost powered > killSprite scoreChange=40
        hungry ghost  > killSprite scoreChange=-1
        power pacman > killSprite scoreChange=10
        pellet pacman > killSprite scoreChange=1
        fruit pacman > killSprite scoreChange=5
        hungry  power > transformTo stype=powered
        powered ghost > transformTo stype=hungry
        
    LevelMapping
        0 > power
        . > pellet
        A > hungry
        1 > redspawn bluespawn pinkspawn orangespawn red blue pink orange
        F > fruit
        
    TerminationSet
        SpriteCounter stype=food   win=True     
        SpriteCounter stype=pacman win=False   """

let aliens_text = 
    """wwwwwwwwwwwwwwwwwwwwwwwwwwww
w............ww............w
w.wwww.wwwwwFww.wwwww.wwww.w
w0wwww.wwwww.ww.wwwww.wwww0w
w.wwww.wwwww.ww.wwwww.wwww.w
w..........................w
w.wwww.ww.wwwwwwww.ww.wwww.w
w.wwww.ww.wwwwwwww.ww.wwww.w
w......ww....ww....ww......w
wwwwww.wwwww ww wwwww.wwwwww
wwwwww.wwwww ww wwwww.wwwwww
wwwwww.ww          ww.wwwwww
wwwwww.ww          ww.wwwwww
wwwwww.ww www  www ww.wwwwww
      F   ww1234ww   F
wwwwww.ww wwwwwwww ww.wwwwww
wwwwww.ww          ww.wwwwww
wwwwww.ww          ww.wwwwww
wwwwww.ww wwwwwwww ww.wwwwww
wwwwww.ww wwwwwwww ww.wwwwww
w............ww............w
w.wwww.wwwww.ww.wwwww.wwww.w
w0wwww.wwwww.ww.wwwww.wwww0w
w...ww.......A........ww...w
www.ww.ww.wwwwwwww.ww.ww.www
www.ww.ww.wwwwwwww.ww.ww.www
w......ww....ww....ww......w
w.wwwwwwwwww.ww.wwwwwwwwww.w
w.wwwwwwwwww.wwFwwwwwwwwww.w
w..........................w
wwwwwwwwwwwwwwwwwwwwwwwwwwww"""

let outcome = ref (0,Continue)
let g = new VGDLGame(aliens_spec,aliens_text,outcome)
g.Run()
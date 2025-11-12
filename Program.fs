open System
open System.Collections.Generic
open System.Threading

// ===== Types =====

type Element =
    | EMPTY
    // powders
    | SAND | GUNPOWDER | ASH | SNOW
    // liquids
    | WATER | SALTWATER | OIL | ETHANOL | ACID | LAVA | MERCURY
    // solids / terrain
    | STONE | GLASS | WALL | WOOD | PLANT | METAL | WIRE | ICE | COAL
    | DIRT | WET_DIRT | SEAWEED
    // gases
    | SMOKE | STEAM | GAS | TOXIC_GAS | HYDROGEN | CHLORINE
    // actors / special
    | FIRE | LIGHTNING | HUMAN | ZOMBIE

type Cell =
    { mutable Type : Element
      mutable Life : int }

type Category =
    | POWDERS | LIQUIDS | SOLIDS | GASES | SPECIAL | CREDITS

type MenuItem =
    { Type  : Element
      Cat   : Category
      Label : string
      Desc  : string }

// ===== Globals & Helpers =====

module Program =

    let mutable gWidth  = 0
    let mutable gHeight = 0
    let mutable grid : Cell[,] = Array2D.zeroCreate 0 0

    let rng = Random()
    let mutable lastBeep = DateTime.UtcNow

    let clamp v mn mx =
        if v < mn then mn
        elif v > mx then mx
        else v

    let inBounds x y =
        x >= 0 && x < gWidth && y >= 0 && y < gHeight

    let rint a b = rng.Next(a, b + 1)
    let chance p = rng.Next(1, 101) <= p
    let empty (c: Cell) = c.Type = Element.EMPTY

    let sandLike e =
        e = Element.SAND || e = Element.GUNPOWDER || e = Element.ASH || e = Element.SNOW

    let liquid e =
        match e with
        | Element.WATER
        | Element.SALTWATER
        | Element.OIL
        | Element.ETHANOL
        | Element.ACID
        | Element.LAVA
        | Element.MERCURY -> true
        | _ -> false

    let solid e =
        match e with
        | Element.STONE
        | Element.GLASS
        | Element.WALL
        | Element.WOOD
        | Element.PLANT
        | Element.METAL
        | Element.WIRE
        | Element.ICE
        | Element.COAL
        | Element.DIRT
        | Element.WET_DIRT
        | Element.SEAWEED -> true
        | _ -> false

    let gas e =
        match e with
        | Element.SMOKE
        | Element.STEAM
        | Element.GAS
        | Element.TOXIC_GAS
        | Element.HYDROGEN
        | Element.CHLORINE -> true
        | _ -> false

    let flammable e =
        e = Element.WOOD || e = Element.PLANT || e = Element.OIL ||
        e = Element.ETHANOL || e = Element.GUNPOWDER || e = Element.COAL ||
        e = Element.SEAWEED

    let conductor e =
        e = Element.METAL || e = Element.WIRE || e = Element.MERCURY || e = Element.SALTWATER

    let dissolvable e =
        e = Element.SAND || e = Element.STONE || e = Element.GLASS ||
        e = Element.WOOD || e = Element.PLANT || e = Element.METAL ||
        e = Element.WIRE || e = Element.ASH || e = Element.COAL ||
        e = Element.SEAWEED || e = Element.DIRT || e = Element.WET_DIRT

    let density e =
        match e with
        | Element.ETHANOL   -> 85
        | Element.OIL       -> 90
        | Element.GAS
        | Element.HYDROGEN  -> 1
        | Element.STEAM     -> 2
        | Element.SMOKE     -> 3
        | Element.CHLORINE  -> 5
        | Element.WATER     -> 100
        | Element.SALTWATER -> 103
        | Element.ACID      -> 110
        | Element.LAVA      -> 160
        | Element.MERCURY   -> 200
        | _                 -> 999

    let nameOf e =
        match e with
        | Element.EMPTY      -> "Empty"
        | Element.SAND       -> "Sand"
        | Element.GUNPOWDER  -> "Gunpowder"
        | Element.ASH        -> "Ash"
        | Element.SNOW       -> "Snow"
        | Element.WATER      -> "Water"
        | Element.SALTWATER  -> "Salt Water"
        | Element.OIL        -> "Oil"
        | Element.ETHANOL    -> "Ethanol"
        | Element.ACID       -> "Acid"
        | Element.LAVA       -> "Lava"
        | Element.MERCURY    -> "Mercury"
        | Element.STONE      -> "Stone"
        | Element.GLASS      -> "Glass"
        | Element.WALL       -> "Wall"
        | Element.WOOD       -> "Wood"
        | Element.PLANT      -> "Plant"
        | Element.METAL      -> "Metal"
        | Element.WIRE       -> "Wire"
        | Element.ICE        -> "Ice"
        | Element.COAL       -> "Coal"
        | Element.DIRT       -> "Dirt"
        | Element.WET_DIRT   -> "Wet Dirt"
        | Element.SEAWEED    -> "Seaweed"
        | Element.SMOKE      -> "Smoke"
        | Element.STEAM      -> "Steam"
        | Element.GAS        -> "Gas"
        | Element.TOXIC_GAS  -> "Toxic Gas"
        | Element.HYDROGEN   -> "Hydrogen"
        | Element.CHLORINE   -> "Chlorine"
        | Element.FIRE       -> "Fire"
        | Element.LIGHTNING  -> "Lightning"
        | Element.HUMAN      -> "Human"
        | Element.ZOMBIE     -> "Zombie"

    let colorOf e =
        match e with
        | Element.EMPTY
            -> ConsoleColor.DarkGray

        | Element.SAND
        | Element.GUNPOWDER
        | Element.SNOW
        | Element.DIRT
            -> ConsoleColor.Yellow

        | Element.WATER
        | Element.SALTWATER
        | Element.STEAM
        | Element.ICE
        | Element.ETHANOL
            -> ConsoleColor.Cyan

        | Element.STONE
        | Element.GLASS
        | Element.WALL
        | Element.METAL
        | Element.WIRE
        | Element.COAL
        | Element.WET_DIRT
            -> ConsoleColor.White

        | Element.WOOD
        | Element.PLANT
        | Element.SEAWEED
        | Element.HUMAN
            -> ConsoleColor.Green

        | Element.FIRE
        | Element.LAVA
        | Element.ZOMBIE
            -> ConsoleColor.Red

        | Element.SMOKE
        | Element.ASH
        | Element.GAS
        | Element.HYDROGEN
            -> ConsoleColor.Magenta

        | Element.OIL
        | Element.MERCURY
            -> ConsoleColor.Blue

        | Element.ACID
        | Element.TOXIC_GAS
        | Element.CHLORINE
        | Element.LIGHTNING
            -> ConsoleColor.DarkYellow

        | _ -> ConsoleColor.Gray

    let glyphOf e =
        match e with
        | Element.EMPTY      -> ' '
        | Element.SAND       -> '.'
        | Element.GUNPOWDER  -> '%'
        | Element.ASH        -> ';'
        | Element.SNOW       -> ','
        | Element.WATER      -> '~'
        | Element.SALTWATER  -> ':'
        | Element.OIL        -> 'o'
        | Element.ETHANOL    -> 'e'
        | Element.ACID       -> 'a'
        | Element.LAVA       -> 'L'
        | Element.MERCURY    -> 'm'
        | Element.STONE      -> '#'
        | Element.GLASS      -> '='
        | Element.WALL       -> '@'
        | Element.WOOD       -> 'w'
        | Element.PLANT      -> 'p'
        | Element.SEAWEED    -> 'v'
        | Element.METAL      -> 'M'
        | Element.WIRE       -> '-'
        | Element.ICE        -> 'I'
        | Element.COAL       -> 'c'
        | Element.DIRT       -> 'd'
        | Element.WET_DIRT   -> 'D'
        | Element.SMOKE      -> '^'
        | Element.STEAM      -> '"'
        | Element.GAS        -> '`'
        | Element.TOXIC_GAS  -> 'x'
        | Element.HYDROGEN   -> '\''
        | Element.CHLORINE   -> 'X'
        | Element.FIRE       -> '*'
        | Element.LIGHTNING  -> '|'
        | Element.HUMAN      -> 'Y'
        | Element.ZOMBIE     -> 'T'

    // ===== Grid =====

    let initGrid w h =
        gWidth  <- w
        gHeight <- h
        grid <- Array2D.init gHeight gWidth (fun _ _ -> { Type = Element.EMPTY; Life = 0 })

    let clearGrid () =
        for y = 0 to gHeight - 1 do
            for x = 0 to gWidth - 1 do
                let c = grid.[y, x]
                c.Type <- Element.EMPTY
                c.Life <- 0

    let swapCells x1 y1 x2 y2 =
        let a = grid.[y1, x1]
        let b = grid.[y2, x2]
        let tType = a.Type
        let tLife = a.Life
        a.Type <- b.Type
        a.Life <- b.Life
        b.Type <- tType
        b.Life <- tLife

    let playBeep freq ms =
        let now = DateTime.UtcNow
        if (now - lastBeep).TotalMilliseconds >= 50.0 then
            lastBeep <- now
            try
                Console.Beep(freq, ms)
            with _ -> ()

    let explode cx cy r =
        playBeep 120 80
        for dy = -r to r do
            for dx = -r to r do
                let x = cx + dx
                let y = cy + dy
                if inBounds x y && dx * dx + dy * dy <= r * r then
                    let c = grid.[y, x]
                    if c.Type <> Element.WALL &&
                       c.Type <> Element.STONE &&
                       c.Type <> Element.GLASS &&
                       c.Type <> Element.METAL &&
                       c.Type <> Element.WIRE &&
                       c.Type <> Element.ICE then
                        let roll = rint 1 100
                        if roll <= 50 then
                            c.Type <- Element.FIRE
                            c.Life <- 15 + rint 0 10
                        elif roll <= 80 then
                            c.Type <- Element.SMOKE
                            c.Life <- 20
                        else
                            c.Type <- Element.GAS
                            c.Life <- 20

    let placeBrush cx cy rad e =
        if e = Element.LIGHTNING then
            if inBounds cx cy then
                playBeep 1000 40
                let x = cx
                let mutable y = cy
                let mutable stop = false
                while y + 1 < gHeight && not stop do
                    let belowCell = grid.[y + 1, x]
                    let below = belowCell.Type
                    if not (empty belowCell) && not (gas below) then
                        stop <- true
                    else
                        y <- y + 1
                for yy = cy to y do
                    let c = grid.[yy, x]
                    c.Type <- Element.LIGHTNING
                    c.Life <- 2
        else
            let r2 = rad * rad
            for dy = -rad to rad do
                for dx = -rad to rad do
                    let x = cx + dx
                    let y = cy + dy
                    if inBounds x y && dx * dx + dy * dy <= r2 then
                        let c = grid.[y, x]
                        c.Type <- e
                        c.Life <- 0
                        if gas e then c.Life <- 25
                        if e = Element.FIRE then c.Life <- 20

    // ===== Simulation =====

    let stepSim () =
        if gWidth <= 0 || gHeight <= 0 then () else
        let updated = Array2D.create gHeight gWidth false
        for y = gHeight - 1 downto 0 do
            for x = 0 to gWidth - 1 do
                if not updated.[y, x] then
                    let cell = grid.[y, x]
                    let t = cell.Type

                    let swapTo nx ny =
                        swapCells x y nx ny
                        updated.[ny, nx] <- true

                    if t = Element.EMPTY || t = Element.WALL then
                        updated.[y, x] <- true

                    // powders
                    elif sandLike t then
                        let mutable moved = false

                        if inBounds x (y + 1) then
                            let below = grid.[y + 1, x]
                            if empty below || liquid below.Type then
                                swapTo x (y + 1)
                                moved <- true

                        if not moved then
                            let dir = if rint 0 1 = 1 then 1 else -1
                            let mutable i = 0
                            while i < 2 && not moved do
                                let nx = x + (if i <> 0 then -dir else dir)
                                let ny = y + 1
                                if inBounds nx ny then
                                    let d = grid.[ny, nx]
                                    if empty d || liquid d.Type then
                                        swapTo nx ny
                                        moved <- true
                                i <- i + 1

                        if not moved then updated.[y, x] <- true

                        // snow melts near heat
                        if t = Element.SNOW then
                            for dy = -1 to 1 do
                                for dx = -1 to 1 do
                                    let nx = x + dx
                                    let ny = y + dy
                                    if inBounds nx ny then
                                        let ne = grid.[ny, nx].Type
                                        if ne = Element.FIRE || ne = Element.LAVA then
                                            cell.Type <- Element.WATER
                                            cell.Life <- 0

                        // seaweed growth: sand under persistent water
                        if t = Element.SAND then
                            if inBounds x (y - 1) && grid.[y - 1, x].Type = Element.WATER then
                                cell.Life <- cell.Life + 1
                                if cell.Life > 200 && inBounds x (y - 1)
                                   && grid.[y - 1, x].Type = Element.WATER then
                                    grid.[y - 1, x].Type <- Element.SEAWEED
                                    grid.[y - 1, x].Life <- 0
                                    cell.Life <- 0
                            else
                                cell.Life <- 0

                    // liquids
                    elif liquid t then
                        let mutable moved = false

                        if inBounds x (y + 1) then
                            let b = grid.[y + 1, x]
                            if empty b || gas b.Type then
                                swapTo x (y + 1)
                                moved <- true
                            elif liquid b.Type && density t > density b.Type then
                                swapTo x (y + 1)
                                moved <- true

                        if not moved then
                            let order = [| -1; 1 |]
                            if rint 0 1 = 1 then
                                let tmp = order.[0]
                                order.[0] <- order.[1]
                                order.[1] <- tmp
                            let mutable i = 0
                            while i < 2 && not moved do
                                let nx = x + order.[i]
                                if inBounds nx y then
                                    let s = grid.[y, nx]
                                    if empty s || gas s.Type then
                                        swapTo nx y
                                        moved <- true
                                    elif liquid s.Type && density t > density s.Type && chance 50 then
                                        swapTo nx y
                                        moved <- true
                                i <- i + 1

                        if not moved then updated.[y, x] <- true

                        // liquid interactions
                        for dy = -1 to 1 do
                            for dx = -1 to 1 do
                                if not (dx = 0 && dy = 0) then
                                    let nx = x + dx
                                    let ny = y + dy
                                    if inBounds nx ny then
                                        let n = grid.[ny, nx]

                                        // water vs fire/lava
                                        if t = Element.WATER || t = Element.SALTWATER then
                                            if n.Type = Element.FIRE then
                                                n.Type <- Element.SMOKE
                                                n.Life <- 15
                                            elif n.Type = Element.LAVA then
                                                n.Type <- Element.STONE
                                                n.Life <- 0
                                                cell.Type <- Element.STEAM
                                                cell.Life <- 20

                                        // oil/ethanol ignite
                                        if t = Element.OIL || t = Element.ETHANOL then
                                            if n.Type = Element.FIRE || n.Type = Element.LAVA then
                                                cell.Type <- Element.FIRE
                                                cell.Life <- 25

                                        // acid eats stuff
                                        if t = Element.ACID then
                                            if dissolvable n.Type then
                                                if chance 30 then
                                                    n.Type <- Element.TOXIC_GAS
                                                    n.Life <- 25
                                                else
                                                    n.Type <- Element.EMPTY
                                                    n.Life <- 0
                                                if chance 25 then
                                                    cell.Type <- Element.EMPTY
                                                    cell.Life <- 0

                                            if n.Type = Element.WATER && chance 30 then
                                                cell.Type <- Element.SALTWATER
                                                cell.Life <- 0
                                                if chance 30 then
                                                    n.Type <- Element.STEAM
                                                    n.Life <- 20

                                        // lava
                                        if t = Element.LAVA then
                                            if flammable n.Type then
                                                n.Type <- Element.FIRE
                                                n.Life <- 25
                                            elif n.Type = Element.SAND || n.Type = Element.SNOW then
                                                n.Type <- Element.GLASS
                                                n.Life <- 0
                                            elif n.Type = Element.WATER || n.Type = Element.SALTWATER then
                                                n.Type <- Element.STONE
                                                n.Life <- 0
                                                cell.Type <- Element.STEAM
                                                cell.Life <- 20
                                            elif n.Type = Element.ICE then
                                                n.Type <- Element.WATER
                                                n.Life <- 0

                        // lava cools
                        if t = Element.LAVA then
                            cell.Life <- cell.Life + 1
                            if cell.Life > 200 then
                                cell.Type <- Element.STONE
                                cell.Life <- 0

                        // hydrate dirt
                        if t = Element.WATER || t = Element.SALTWATER then
                            for dy = -1 to 1 do
                                for dx = -1 to 1 do
                                    let nx = x + dx
                                    let ny = y + dy
                                    if inBounds nx ny then
                                        let n = grid.[ny, nx]
                                        if n.Type = Element.DIRT || n.Type = Element.WET_DIRT then
                                            n.Type <- Element.WET_DIRT
                                            n.Life <- 300

                    // gases
                    elif gas t then
                        let mutable moved = false

                        let tries = if t = Element.HYDROGEN then 2 else 1
                        let mutable iTry = 0
                        while iTry < tries && not moved do
                            if inBounds x (y - 1) && empty grid.[y - 1, x] then
                                swapTo x (y - 1)
                                moved <- true
                            iTry <- iTry + 1

                        if not moved then
                            let order = [| -1; 1 |]
                            if rint 0 1 = 1 then
                                let tmp = order.[0]
                                order.[0] <- order.[1]
                                order.[1] <- tmp
                            let mutable i = 0
                            while i < 2 && not moved do
                                let nx = x + order.[i]
                                let ny = y - (if chance 50 then 1 else 0)
                                if inBounds nx ny && empty grid.[ny, nx] then
                                    swapTo nx ny
                                    moved <- true
                                i <- i + 1

                        // gas reactions
                        if t = Element.HYDROGEN || t = Element.GAS then
                            for dy = -1 to 1 do
                                for dx = -1 to 1 do
                                    if not (dx = 0 && dy = 0) then
                                        let nx = x + dx
                                        let ny = y + dy
                                        if inBounds nx ny then
                                            let ne = grid.[ny, nx].Type
                                            if ne = Element.FIRE || ne = Element.LAVA then
                                                if t = Element.HYDROGEN then
                                                    explode x y 4
                                                else
                                                    cell.Type <- Element.FIRE
                                                    cell.Life <- 12

                        if t = Element.CHLORINE then
                            for dy = -1 to 1 do
                                for dx = -1 to 1 do
                                    let nx = x + dx
                                    let ny = y + dy
                                    if inBounds nx ny then
                                        if grid.[ny, nx].Type = Element.PLANT && chance 35 then
                                            let n = grid.[ny, nx]
                                            n.Type <- Element.TOXIC_GAS
                                            n.Life <- 25

                        // gas lifetime & condensation
                        cell.Life <- cell.Life - 1
                        if cell.Life <= 0 then
                            if t = Element.STEAM && chance 35 then
                                cell.Type <- Element.WATER
                                cell.Life <- 0
                            elif t = Element.SMOKE && chance 30 then
                                cell.Type <- Element.ASH
                                cell.Life <- 0
                            else
                                cell.Type <- Element.EMPTY
                                cell.Life <- 0
                        else
                            if not moved then updated.[y, x] <- true

                    // FIRE
                    elif t = Element.FIRE then
                        if inBounds x (y - 1) &&
                           (empty grid.[y - 1, x] || gas grid.[y - 1, x].Type) &&
                           chance 50 then
                            swapTo x (y - 1)

                        for dy = -1 to 1 do
                            for dx = -1 to 1 do
                                if not (dx = 0 && dy = 0) then
                                    let nx = x + dx
                                    let ny = y + dy
                                    if inBounds nx ny then
                                        let n = grid.[ny, nx]
                                        if flammable n.Type && chance 40 then
                                            if n.Type = Element.GUNPOWDER then
                                                explode nx ny 5
                                            else
                                                n.Type <- Element.FIRE
                                                n.Life <- 15 + rint 0 10
                                        if n.Type = Element.WATER || n.Type = Element.SALTWATER then
                                            cell.Type <- Element.SMOKE
                                            cell.Life <- 15
                                        if n.Type = Element.WIRE || n.Type = Element.METAL then
                                            if chance 5 then
                                                n.Life <- max n.Life 5

                        cell.Life <- cell.Life - 1
                        if cell.Life <= 0 then
                            cell.Type <- Element.SMOKE
                            cell.Life <- 15
                        updated.[y, x] <- true

                    // LIGHTNING
                    elif t = Element.LIGHTNING then
                        for dy = -2 to 2 do
                            for dx = -2 to 2 do
                                if not (dx = 0 && dy = 0) then
                                    let nx = x + dx
                                    let ny = y + dy
                                    if inBounds nx ny then
                                        let n = grid.[ny, nx]
                                        let ne = n.Type
                                        if ne = Element.WIRE || ne = Element.METAL || ne = Element.SALTWATER then
                                            n.Life <- max n.Life 12
                                        if flammable ne then
                                            if ne = Element.GUNPOWDER then
                                                explode nx ny 6
                                            else
                                                n.Type <- Element.FIRE
                                                n.Life <- 20 + rint 0 10
                                        if ne = Element.HYDROGEN || ne = Element.GAS then
                                            explode nx ny 4

                        cell.Life <- cell.Life - 1
                        if cell.Life <= 0 then
                            cell.Type <- Element.EMPTY
                            cell.Life <- 0
                        updated.[y, x] <- true

                    else
                        // helper for HUMAN/ZOMBIE movement
                        let walkTry tx ty =
                            if not (inBounds tx ty) then false
                            else
                                let d = grid.[ty, tx]
                                if empty d || gas d.Type then
                                    swapCells x y tx ty
                                    true
                                else false

                        // HUMAN
                        if t = Element.HUMAN then
                            cell.Life <- cell.Life + 1

                            // hazards kill human
                            let mutable dead = false
                            for dy = -1 to 1 do
                                for dx = -1 to 1 do
                                    if not dead then
                                        let nx = x + dx
                                        let ny = y + dy
                                        if inBounds nx ny then
                                            let ne = grid.[ny, nx].Type
                                            if ne = Element.FIRE ||
                                               ne = Element.LAVA ||
                                               ne = Element.ACID ||
                                               ne = Element.TOXIC_GAS ||
                                               ne = Element.CHLORINE then
                                                cell.Type <- Element.FIRE
                                                cell.Life <- 10
                                                dead <- true
                            if dead then
                                updated.[y, x] <- true
                            else
                                // gravity
                                if inBounds x (y + 1) then
                                    let b = grid.[y + 1, x].Type
                                    if empty grid.[y + 1, x] || gas b then
                                        swapTo x (y + 1)
                                    else
                                        // look for nearest zombie
                                        let mutable zx = 0
                                        let mutable zy = 0
                                        let mutable seen = false
                                        for ry = -6 to 6 do
                                            for rx = -6 to 6 do
                                                if not seen then
                                                    let nx = x + rx
                                                    let ny = y + ry
                                                    if inBounds nx ny &&
                                                       grid.[ny, nx].Type = Element.ZOMBIE then
                                                        zx <- nx
                                                        zy <- ny
                                                        seen <- true
                                        // attack adjacent zombies
                                        for dy = -1 to 1 do
                                            for dx = -1 to 1 do
                                                if not (dx = 0 && dy = 0) then
                                                    let nx = x + dx
                                                    let ny = y + dy
                                                    if inBounds nx ny &&
                                                       grid.[ny, nx].Type = Element.ZOMBIE &&
                                                       chance 35 then
                                                        if chance 60 then
                                                            grid.[ny, nx].Type <- Element.FIRE
                                                            grid.[ny, nx].Life <- 10 + rint 0 10
                                                        else
                                                            grid.[ny, nx].Type <- Element.ASH
                                                            grid.[ny, nx].Life <- 0
                                        // flee zombie
                                        let mutable dir = if rint 0 1 = 1 then 1 else -1
                                        if seen then
                                            dir <- if zx < x then 1 else -1
                                        if not (walkTry (x + dir) y) then
                                            // small jump
                                            if inBounds (x + dir) (y - 1) &&
                                               empty grid.[y - 1, x + dir] &&
                                               empty grid.[y - 1, x] &&
                                               chance 70 then
                                                swapCells x y x (y - 1)
                                            else
                                                let dir2 = if rint 0 1 = 1 then 1 else -1
                                                ignore (walkTry (x + dir2) y)
                                        updated.[y, x] <- true

                        // ZOMBIE
                        elif t = Element.ZOMBIE then
                            cell.Life <- cell.Life + 1

                            let mutable killed = false
                            for dy = -1 to 1 do
                                for dx = -1 to 1 do
                                    if not killed then
                                        let nx = x + dx
                                        let ny = y + dy
                                        if inBounds nx ny then
                                            let ne = grid.[ny, nx].Type
                                            if ne = Element.FIRE || ne = Element.LAVA || ne = Element.ACID then
                                                cell.Type <- Element.FIRE
                                                cell.Life <- 15
                                                killed <- true
                            if killed then
                                updated.[y, x] <- true
                            else
                                // gravity
                                if inBounds x (y + 1) then
                                    let b = grid.[y + 1, x].Type
                                    if empty grid.[y + 1, x] || gas b then
                                        swapTo x (y + 1)
                                    else
                                        // look for human
                                        let mutable hx = 0
                                        let mutable hy = 0
                                        let mutable seen = false
                                        for ry = -6 to 6 do
                                            for rx = -6 to 6 do
                                                if not seen then
                                                    let nx = x + rx
                                                    let ny = y + ry
                                                    if inBounds nx ny &&
                                                       grid.[ny, nx].Type = Element.HUMAN then
                                                        hx <- nx
                                                        hy <- ny
                                                        seen <- true
                                        // infect/attack neighboring humans
                                        for dy = -1 to 1 do
                                            for dx = -1 to 1 do
                                                if not (dx = 0 && dy = 0) then
                                                    let nx = x + dx
                                                    let ny = y + dy
                                                    if inBounds nx ny &&
                                                       grid.[ny, nx].Type = Element.HUMAN then
                                                        if chance 70 then
                                                            grid.[ny, nx].Type <- Element.ZOMBIE
                                                            grid.[ny, nx].Life <- 0
                                                        else
                                                            grid.[ny, nx].Type <- Element.FIRE
                                                            grid.[ny, nx].Life <- 10
                                        let mutable dir2 =
                                            if seen then (if hx > x then 1 else -1)
                                            else (if rint 0 1 = 1 then 1 else -1)
                                        if not (walkTry (x + dir2) y) then
                                            if inBounds (x + dir2) (y - 1) &&
                                               empty grid.[y - 1, x + dir2] &&
                                               empty grid.[y - 1, x] &&
                                               chance 70 then
                                                swapCells x y x (y - 1)
                                            else
                                                let dir3 = if rint 0 1 = 1 then 1 else -1
                                                ignore (walkTry (x + dir3) y)
                                        updated.[y, x] <- true

                        // WET_DIRT drying
                        elif t = Element.WET_DIRT then
                            let mutable nearWater = false
                            for dy = -1 to 1 do
                                for dx = -1 to 1 do
                                    if not nearWater then
                                        let nx = x + dx
                                        let ny = y + dy
                                        if inBounds nx ny then
                                            let ne = grid.[ny, nx].Type
                                            if ne = Element.WATER || ne = Element.SALTWATER then
                                                nearWater <- true
                            if not nearWater then
                                cell.Life <- cell.Life - 1
                                if cell.Life <= 0 then
                                    cell.Type <- Element.DIRT
                                    cell.Life <- 0
                            updated.[y, x] <- true

                        // PLANT / SEAWEED
                        elif t = Element.PLANT || t = Element.SEAWEED then
                            for dy = -1 to 1 do
                                for dx = -1 to 1 do
                                    if not (dx = 0 && dy = 0) then
                                        let nx = x + dx
                                        let ny = y + dy
                                        if inBounds nx ny then
                                            if grid.[ny, nx].Type = Element.FIRE ||
                                               grid.[ny, nx].Type = Element.LAVA then
                                                cell.Type <- Element.FIRE
                                                cell.Life <- 20
                            if cell.Type = Element.FIRE then
                                updated.[y, x] <- true
                            else
                                if t = Element.PLANT then
                                    let goodSoil =
                                        inBounds x (y + 1) &&
                                        grid.[y + 1, x].Type = Element.WET_DIRT
                                    if goodSoil && chance 3 then
                                        let gx = x + rint -1 1
                                        let gy = y - 1
                                        if inBounds gx gy && empty grid.[gy, gx] then
                                            grid.[gy, gx].Type <- Element.PLANT
                                            grid.[gy, gx].Life <- 0
                                else
                                    let mutable underWater = false
                                    if inBounds x (y - 1) then
                                        let above = grid.[y - 1, x].Type
                                        underWater <-
                                            above = Element.WATER ||
                                            above = Element.SALTWATER ||
                                            above = Element.STEAM
                                    if underWater && chance 3 then
                                        let gx = x + rint -1 1
                                        let gy = y - 1
                                        if inBounds gx gy &&
                                           (grid.[gy, gx].Type = Element.WATER ||
                                            grid.[gy, gx].Type = Element.SALTWATER) then
                                            grid.[gy, gx].Type <- Element.SEAWEED
                                updated.[y, x] <- true

                        // WOOD / COAL burn
                        elif t = Element.WOOD || t = Element.COAL then
                            for dy = -1 to 1 do
                                for dx = -1 to 1 do
                                    if not (dx = 0 && dy = 0) then
                                        let nx = x + dx
                                        let ny = y + dy
                                        if inBounds nx ny then
                                            if grid.[ny, nx].Type = Element.FIRE ||
                                               grid.[ny, nx].Type = Element.LAVA then
                                                cell.Type <- Element.FIRE
                                                cell.Life <- (if t = Element.COAL then 35 else 25)
                            updated.[y, x] <- true

                        // GUNPOWDER
                        elif t = Element.GUNPOWDER then
                            for dy = -1 to 1 do
                                for dx = -1 to 1 do
                                    if not (dx = 0 && dy = 0) then
                                        let nx = x + dx
                                        let ny = y + dy
                                        if inBounds nx ny then
                                            let ne = grid.[ny, nx].Type
                                            if ne = Element.FIRE || ne = Element.LAVA then
                                                explode x y 5
                            updated.[y, x] <- true

                        // WIRE / METAL conduction
                        elif t = Element.WIRE || t = Element.METAL then
                            if cell.Life > 0 then
                                let q = cell.Life
                                for dy = -1 to 1 do
                                    for dx = -1 to 1 do
                                        if not (dx = 0 && dy = 0) then
                                            let nx = x + dx
                                            let ny = y + dy
                                            if inBounds nx ny then
                                                let n = grid.[ny, nx]
                                                if n.Type = Element.WIRE || n.Type = Element.METAL then
                                                    if n.Life < q - 1 then
                                                        n.Life <- q - 1
                                                if flammable n.Type && chance 15 then
                                                    if n.Type = Element.GUNPOWDER then
                                                        explode nx ny 5
                                                    else
                                                        n.Type <- Element.FIRE
                                                        n.Life <- 15 + rint 0 10
                                                if n.Type = Element.HYDROGEN || n.Type = Element.GAS then
                                                    if chance 35 then explode nx ny 4
                                cell.Life <- cell.Life - 1
                                if cell.Life < 0 then cell.Life <- 0
                            updated.[y, x] <- true

                        // ICE
                        elif t = Element.ICE then
                            for dy = -1 to 1 do
                                for dx = -1 to 1 do
                                    let nx = x + dx
                                    let ny = y + dy
                                    if inBounds nx ny then
                                        let ne = grid.[ny, nx].Type
                                        if ne = Element.FIRE ||
                                           ne = Element.LAVA ||
                                           ne = Element.STEAM then
                                            if chance 25 then
                                                cell.Type <- Element.WATER
                                                cell.Life <- 0
                            updated.[y, x] <- true

                        // default static
                        else
                            updated.[y, x] <- true

    // ===== Drawing =====

    let drawGrid cx cy cur paused brush =
        Console.SetCursorPosition(0, 0)
        for y = 0 to gHeight - 1 do
            for x = 0 to gWidth - 1 do
                let c = grid.[y, x]
                let mutable ch = glyphOf c.Type
                if c.Type = Element.HUMAN then
                    ch <- if (c.Life / 6) % 2 <> 0 then 'y' else 'Y'
                if c.Type = Element.ZOMBIE then
                    ch <- if (c.Life / 6) % 2 <> 0 then 't' else 'T'
                if c.Type = Element.LIGHTNING then
                    ch <- '|'
                if x = cx && y = cy then
                    ch <- '+'
                Console.ForegroundColor <- colorOf c.Type
                Console.Write ch
            if gWidth < Console.WindowWidth then
                Console.ForegroundColor <- ConsoleColor.Gray
                Console.Write(String(' ', Console.WindowWidth - gWidth))

        let maxx = Console.WindowWidth
        let maxy = Console.WindowHeight

        let widthLine = if gWidth > 0 then gWidth else maxx
        let separator = String('-', min maxx widthLine)

        if gHeight < maxy then
            Console.ForegroundColor <- ConsoleColor.DarkGray
            Console.Write separator
            if separator.Length < maxx then
                Console.Write(String(' ', maxx - separator.Length))

        let mutable status =
            "Move: Arrows/WASD | Space: draw | E: erase | +/-: brush | C/X: clear | P: pause | M/Tab: elements | Q: quit"
        if status.Length > maxx then status <- status.Substring(0, maxx)

        if gHeight + 1 < maxy then
            Console.ForegroundColor <- ConsoleColor.Gray
            Console.Write status
            if status.Length < maxx then
                Console.Write(String(' ', maxx - status.Length))

        let mutable info =
            "Current: " + nameOf cur + " | Brush r=" + string brush + (if paused then " [PAUSED]" else "")
        if info.Length > maxx then info <- info.Substring(0, maxx)

        if gHeight + 2 < maxy then
            Console.ForegroundColor <- ConsoleColor.White
            Console.Write info
            if info.Length < maxx then
                Console.Write(String(' ', maxx - info.Length))

    // ===== Menu & Credits =====

    let MENU : MenuItem[] =
        [|
            // Powders
            { Type = Element.SAND      ; Cat = Category.POWDERS; Label = "Sand"     ; Desc = "Classic falling grains." }
            { Type = Element.GUNPOWDER ; Cat = Category.POWDERS; Label = "Gunpowder"; Desc = "Explodes when ignited." }
            { Type = Element.ASH       ; Cat = Category.POWDERS; Label = "Ash"      ; Desc = "Burnt residue." }
            { Type = Element.SNOW      ; Cat = Category.POWDERS; Label = "Snow"     ; Desc = "Melts near heat." }

            // Liquids
            { Type = Element.WATER     ; Cat = Category.LIQUIDS; Label = "Water"    ; Desc = "Flows, cools, extinguishes." }
            { Type = Element.SALTWATER ; Cat = Category.LIQUIDS; Label = "Salt Water"; Desc = "Conductive water." }
            { Type = Element.OIL       ; Cat = Category.LIQUIDS; Label = "Oil"      ; Desc = "Light, flammable." }
            { Type = Element.ETHANOL   ; Cat = Category.LIQUIDS; Label = "Ethanol"  ; Desc = "Very flammable." }
            { Type = Element.ACID      ; Cat = Category.LIQUIDS; Label = "Acid"     ; Desc = "Dissolves many materials." }
            { Type = Element.LAVA      ; Cat = Category.LIQUIDS; Label = "Lava"     ; Desc = "Hot molten rock." }
            { Type = Element.MERCURY   ; Cat = Category.LIQUIDS; Label = "Mercury"  ; Desc = "Heavy liquid metal." }

            // Solids
            { Type = Element.STONE     ; Cat = Category.SOLIDS ; Label = "Stone"    ; Desc = "Heavy solid block." }
            { Type = Element.GLASS     ; Cat = Category.SOLIDS ; Label = "Glass"    ; Desc = "From sand + lava." }
            { Type = Element.WALL      ; Cat = Category.SOLIDS ; Label = "Wall"     ; Desc = "Indestructible barrier." }
            { Type = Element.WOOD      ; Cat = Category.SOLIDS ; Label = "Wood"     ; Desc = "Flammable solid." }
            { Type = Element.PLANT     ; Cat = Category.SOLIDS ; Label = "Plant"    ; Desc = "Grows on wet dirt." }
            { Type = Element.SEAWEED   ; Cat = Category.SOLIDS ; Label = "Seaweed"  ; Desc = "Grows in water over sand." }
            { Type = Element.METAL     ; Cat = Category.SOLIDS ; Label = "Metal"    ; Desc = "Conductive solid." }
            { Type = Element.WIRE      ; Cat = Category.SOLIDS ; Label = "Wire"     ; Desc = "Conductive path." }
            { Type = Element.ICE       ; Cat = Category.SOLIDS ; Label = "Ice"      ; Desc = "Melts into water." }
            { Type = Element.COAL      ; Cat = Category.SOLIDS ; Label = "Coal"     ; Desc = "Burns longer." }
            { Type = Element.DIRT      ; Cat = Category.SOLIDS ; Label = "Dirt"     ; Desc = "Gets wet; grows plants." }
            { Type = Element.WET_DIRT  ; Cat = Category.SOLIDS ; Label = "Wet Dirt" ; Desc = "Dries over time." }

            // Gases
            { Type = Element.SMOKE     ; Cat = Category.GASES  ; Label = "Smoke"    ; Desc = "Rises; may fall as ash." }
            { Type = Element.STEAM     ; Cat = Category.GASES  ; Label = "Steam"    ; Desc = "Condenses to water." }
            { Type = Element.GAS       ; Cat = Category.GASES  ; Label = "Gas"      ; Desc = "Neutral rising gas." }
            { Type = Element.TOXIC_GAS ; Cat = Category.GASES  ; Label = "Toxic Gas"; Desc = "Nasty chemical cloud." }
            { Type = Element.HYDROGEN  ; Cat = Category.GASES  ; Label = "Hydrogen" ; Desc = "Very light, explosive." }
            { Type = Element.CHLORINE  ; Cat = Category.GASES  ; Label = "Chlorine" ; Desc = "Harms plants." }

            // Special
            { Type = Element.FIRE      ; Cat = Category.SPECIAL; Label = "Fire"     ; Desc = "Burns & flickers upward." }
            { Type = Element.LIGHTNING ; Cat = Category.SPECIAL; Label = "Lightning"; Desc = "Yellow electrical bolt." }
            { Type = Element.HUMAN     ; Cat = Category.SPECIAL; Label = "Human"    ; Desc = "Avoids zombie, fights back." }
            { Type = Element.ZOMBIE    ; Cat = Category.SPECIAL; Label = "Zombie"   ; Desc = "Chases and infects humans." }
            { Type = Element.EMPTY     ; Cat = Category.SPECIAL; Label = "Eraser"   ; Desc = "Place empty space." }

            // Credits
            { Type = Element.EMPTY     ; Cat = Category.CREDITS; Label = "Credits"  ; Desc = "Show credits & license." }
        |]

    let catName c =
        match c with
        | Category.POWDERS -> "Powders"
        | Category.LIQUIDS -> "Liquids"
        | Category.SOLIDS  -> "Solids"
        | Category.GASES   -> "Gases"
        | Category.SPECIAL -> "Special"
        | Category.CREDITS -> "Credits"

    let showCreditsOverlay () =
        let maxy = Console.WindowHeight
        let maxx = Console.WindowWidth
        if maxx < 40 || maxy < 12 then () else

        let w = min (maxx - 4) 70
        let h = min (maxy - 4) 15
        let lx = (maxx - w) / 2
        let ty = (maxy - h) / 2
        let rx = lx + w - 1
        let by = ty + h - 1

        // clear area
        Console.SetCursorPosition(0, 0)
        for _y = 0 to maxy - 1 do
            Console.Write(String(' ', maxx))

        let put (px:int) (py:int) (ch:char) =
            if px >= 0 && px < maxx && py >= 0 && py < maxy then
                Console.SetCursorPosition(px, py)
                Console.Write ch

        put lx ty '+'
        put rx ty '+'
        put lx by '+'
        put rx by '+'
        for x = lx + 1 to rx - 1 do
            put x ty '-'
            put x by '-'
        for y = ty + 1 to by - 1 do
            put lx y '|'
            put rx y '|'

        let title = " Credits "
        let mutable titleX = lx + (w - title.Length) / 2
        if titleX < lx + 1 then titleX <- lx + 1
        Console.SetCursorPosition(titleX, ty)
        Console.Write title

        let lines =
            [|
                "Terminal Powder Toy-like Sandbox"
                "Author: Robert"
                "GitHub: https://github.com/RobertFlexx"
                "Language: F# (.NET console)"
                ""
                "BSD 3-Clause License (snippet):"
                "Redistribution and use in source and binary forms,"
                "with or without modification, are permitted provided"
                "that the following conditions are met:"
                "1) Source redistributions retain this notice & disclaimer."
                "2) Binary redistributions reproduce this notice & disclaimer."
                "3) Names of contributors can't be used to endorse products"
                "   without prior written permission."
                ""
                "Press any key to return."
            |]

        let mutable yy = ty + 2
        for i = 0 to lines.Length - 1 do
            if yy < by then
                Console.SetCursorPosition(lx + 2, yy)
                let line = lines.[i]
                let line2 = if line.Length > w - 4 then line.Substring(0, w - 4) else line
                Console.Write line2
                yy <- yy + 1

        Console.SetCursorPosition(0, maxy - 1)
        let _ = Console.ReadKey(true)
        ()

    let elementMenu (current: Element) =
        let tabs =
            [|
                Category.POWDERS
                Category.LIQUIDS
                Category.SOLIDS
                Category.GASES
                Category.SPECIAL
                Category.CREDITS
            |]
        let NT = tabs.Length

        let mutable curTab =
            MENU
            |> Array.tryFind (fun it -> it.Type = current)
            |> Option.map (fun it -> it.Cat)
            |> Option.defaultValue Category.POWDERS

        let mutable tabIdx =
            match Array.tryFindIndex ((=) curTab) tabs with
            | Some i -> i
            | None -> 0

        let mutable sel = 0
        let mutable done' = false
        let mutable result = current

        while not done' do
            let maxy = Console.WindowHeight
            let maxx = Console.WindowWidth

            let idx =
                [|
                    for i = 0 to MENU.Length - 1 do
                        if MENU.[i].Cat = tabs.[tabIdx] then
                            yield i
                |]

            if sel < 0 then sel <- 0
            if idx.Length > 0 && sel >= idx.Length then sel <- idx.Length - 1

            let mutable boxW = max 44 (maxx - 6)
            let mutable boxH = max 14 (maxy - 6)
            boxW <- min boxW maxx
            boxH <- min boxH maxy
            let lx = (maxx - boxW) / 2
            let ty = (maxy - boxH) / 2
            let rx = lx + boxW - 1
            let by = ty + boxH - 1

            Console.Clear()

            let put (px:int) (py:int) (ch:char) =
                if px >= 0 && px < maxx && py >= 0 && py < maxy then
                    Console.SetCursorPosition(px, py)
                    Console.Write ch

            put lx ty '+'
            put rx ty '+'
            put lx by '+'
            put rx by '+'
            for x = lx + 1 to rx - 1 do
                put x ty '-'
                put x by '-'
            for y = ty + 1 to by - 1 do
                put lx y '|'
                put rx y '|'

            let title = " Element Browser "
            let mutable titleX = lx + (boxW - title.Length) / 2
            if titleX < lx + 1 then titleX <- lx + 1
            Console.SetCursorPosition(titleX, ty)
            Console.Write title

            let tabsY = ty + 1
            let mutable cx = lx + 2
            for i = 0 to NT - 1 do
                let tab = " " + catName tabs.[i] + " "
                if cx + tab.Length < rx then
                    Console.SetCursorPosition(cx, tabsY)
                    if i = tabIdx then
                        Console.BackgroundColor <- ConsoleColor.DarkGray
                        Console.ForegroundColor <- ConsoleColor.White
                    else
                        Console.ResetColor()
                        Console.ForegroundColor <- ConsoleColor.Gray
                    Console.Write tab
                    Console.ResetColor()
                    cx <- cx + tab.Length + 1

            let mutable yList = ty + 3
            let maxListY = by - 3
            Console.ForegroundColor <- ConsoleColor.White
            for i = 0 to idx.Length - 1 do
                if yList <= maxListY then
                    let it = MENU.[idx.[i]]
                    let mutable line = " " + it.Label + " - " + it.Desc
                    if line.Length > boxW - 4 then line <- line.Substring(0, boxW - 4)
                    Console.SetCursorPosition(lx + 2, yList)
                    if i = sel then
                        Console.BackgroundColor <- ConsoleColor.DarkGray
                        Console.ForegroundColor <- ConsoleColor.White
                    else
                        Console.ResetColor()
                        Console.ForegroundColor <- ConsoleColor.Gray
                    Console.Write(line.PadRight(boxW - 4))
                    Console.ResetColor()
                    yList <- yList + 1

            let mutable hint =
                "Left/Right: tabs | Up/Down: select | Enter: choose | ESC: back"
            if hint.Length > boxW - 4 then hint <- hint.Substring(0, boxW - 4)
            Console.SetCursorPosition(lx + 2, by - 1)
            Console.ForegroundColor <- ConsoleColor.DarkGray
            Console.Write(hint.PadRight(boxW - 4))
            Console.ResetColor()

            let keyInfo = Console.ReadKey(true)
            let key = keyInfo.Key
            if key = ConsoleKey.LeftArrow then
                tabIdx <- (tabIdx + NT - 1) % NT
                sel <- 0
            elif key = ConsoleKey.RightArrow then
                tabIdx <- (tabIdx + 1) % NT
                sel <- 0
            elif key = ConsoleKey.UpArrow then
                if idx.Length > 0 then
                    sel <- (sel + idx.Length - 1) % idx.Length
            elif key = ConsoleKey.DownArrow then
                if idx.Length > 0 then
                    sel <- (sel + 1) % idx.Length
            elif key = ConsoleKey.Enter then
                if idx.Length > 0 then
                    let it = MENU.[idx.[sel]]
                    if it.Cat = Category.CREDITS then
                        showCreditsOverlay ()
                    else
                        result <- it.Type
                        done' <- true
                else
                    done' <- true
            elif key = ConsoleKey.Escape then
                done' <- true

        result

    // ===== Main =====

    [<EntryPoint>]
    let main argv =
        Console.OutputEncoding <- Text.Encoding.UTF8
        Console.CursorVisible <- false

        let termW = Console.WindowWidth
        let termH = Console.WindowHeight
        let simH = max 1 (termH - 3)
        initGrid termW simH

        let mutable cx = gWidth / 2
        let mutable cy = gHeight / 2
        let mutable brush = 1
        let mutable current = Element.SAND
        let mutable running = true
        let mutable paused = false

        while running do
            let nw = Console.WindowWidth
            let nh = Console.WindowHeight
            let nSimH = max 1 (nh - 3)
            if nw <> gWidth || nSimH <> gHeight then
                initGrid nw nSimH
                cx <- clamp cx 0 (gWidth - 1)
                cy <- clamp cy 0 (gHeight - 1)
                Console.Clear()

            while Console.KeyAvailable do
                let keyInfo = Console.ReadKey(true)
                let ch = keyInfo.KeyChar
                let key = keyInfo.Key

                if key = ConsoleKey.Q then
                    running <- false
                elif key = ConsoleKey.LeftArrow || ch = 'a' || ch = 'A' then
                    cx <- max 0 (cx - 1)
                elif key = ConsoleKey.RightArrow || ch = 'd' || ch = 'D' then
                    cx <- min (gWidth - 1) (cx + 1)
                elif key = ConsoleKey.UpArrow || ch = 'w' then
                    cy <- max 0 (cy - 1)
                elif key = ConsoleKey.DownArrow || ch = 's' || ch = 'S' then
                    cy <- min (gHeight - 1) (cy + 1)
                elif ch = ' ' then
                    placeBrush cx cy brush current
                elif ch = 'e' || ch = 'E' then
                    placeBrush cx cy brush Element.EMPTY
                elif ch = '+' || ch = '=' then
                    if brush < 8 then brush <- brush + 1
                elif ch = '-' || ch = '_' then
                    if brush > 1 then brush <- brush - 1
                elif ch = 'c' || ch = 'C' || ch = 'x' || ch = 'X' then
                    clearGrid ()
                elif ch = 'p' || ch = 'P' then
                    paused <- not paused
                elif ch = 'm' || ch = 'M' || key = ConsoleKey.Tab then
                    current <- elementMenu current
                    Console.Clear()
                elif ch = '1' then current <- Element.SAND
                elif ch = '2' then current <- Element.WATER
                elif ch = '3' then current <- Element.STONE
                elif ch = '4' then current <- Element.WOOD
                elif ch = '5' then current <- Element.FIRE
                elif ch = '6' then current <- Element.OIL
                elif ch = '7' then current <- Element.LAVA
                elif ch = '8' then current <- Element.PLANT
                elif ch = '9' then current <- Element.GUNPOWDER
                elif ch = '0' then current <- Element.ACID
                elif ch = 'W' then current <- Element.WALL
                elif ch = 'L' then current <- Element.LIGHTNING
                elif ch = 'H' || ch = 'h' then current <- Element.HUMAN
                elif ch = 'Z' then current <- Element.ZOMBIE
                elif ch = 'D' then current <- Element.DIRT

            if not paused then
                stepSim ()

            drawGrid cx cy current paused brush
            Thread.Sleep 16

        Console.ResetColor()
        Console.Clear()
        Console.CursorVisible <- true
        0

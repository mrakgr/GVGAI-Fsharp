// I managed to get Painter to work by accident somehow.
// Due to the way collision detection works, it gives rise to some kind of interesting emergent phenomena in this game it seems.
// By all reason, having ground and paint kill each other should produce empty sprites, but here it flips.

//        paint ground > killSprite # Do not think too much why this works. It is evolutionary programming at its finest. Even I do not get it and I made this.
//        ground paint > killSprite

// Without the two constraints above the program just blows up.

let aliens_spec ="""#(Inspired by) Notsnake: http://www.puzzlescript.net/play.html?p=e2c1c8e99b3f9d9b6edd

BasicGame
    SpriteSet
        
        ground > Immovable color=GRAY
        paint > Immovable color=DARKBLUE
        prepaint > Flicker limit=2 invisible=TRUE
        prepainting > Resource limit=2
        preground > Flicker limit=2 invisible=TRUE
        pregrounding > Resource limit=2

        avatar > MovingAvatar shrinkfactor=0.8

    LevelMapping
        A > avatar ground
        . > ground
        x > paint
        T > ground prepaint
    InteractionSet
        avatar EOS > stepBack

        paint ground > killSprite # Do not think too much why this works. It is evolutionary programming at its finest. Even I do not get it and I made this.
        ground paint > killSprite
        
        prepaint avatar > killSprite
        ground avatar > spawnIfHasMore stype=prepaint resource=null limit=0
        ground prepaint > killIfOtherHasMore resource=prepainting limit=2  scoreChange=1
        prepaint ground > changeResource resource=prepainting value=1
        prepaint ground > spawnIfHasMore resource=prepainting limit=2 stype=paint 
        
        preground avatar > killSprite
        paint avatar > spawnIfHasMore stype=preground resource=null limit=0
        paint preground > killIfOtherHasMore resource=pregrounding limit=2
        preground paint > changeResource resource=pregrounding value=1
        preground paint > spawnIfHasMore resource=pregrounding limit=2 stype=ground

    TerminationSet
        SpriteCounter stype=ground limit=0 win=True"""


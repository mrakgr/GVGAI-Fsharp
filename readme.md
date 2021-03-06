The very first version of the GVGAI-Fsharp.

Aliens, Boulder Dash and the Butterfly games work well currently. Controllers, NPC AI and significant amount of effects are missing for the time being.

I had hope that I would be done by the middle of February, but now on the 20th I see that this is going to take me a while longer. I'll definitely have it all done by the end of March.

Right now, I need to take a break from this in order to learn some .NET assembly in order to better understand F#'s performance. I'll resume work on this before the end of the month.

So far during this project my development has mirrored that of what I had done during the Cool compiler course last year, so it only makes sense to learn some assembly at this stage.

I only decided to do that thing on a whim and never really expected that I would ever write a compiler again.

I had to learn parsing all over again in F#, but I am rather satisfied how things are going so far. Doing this made me realize that I got it all wrong what games were about. Nearly 1k LOC in this project are getting the data into the correct form so the library can use it internally. It is remarkable. So far I am really satisfied how it turned out in terms of design. The more I learn about functional programming the more I love it, it seems.

My goal for this once I am done is to write some proper tutorials regarding the GVGAI-Fsharp library's architecture so that other people can use it and show how the Spiral AD library can be used to enable learning-based controllers. I say 'proper' because my tutorials on the Spiral library so far are clearly no good - literally the only person I can imagine them being useful would be me from a year ago and that is a very limited audience.

I really should be gearing up poker bots instead of doing Atari remakes, but I need a platform where I can easily diagnose the capabilities of the agents I am making. I will definitely need what I am making now to bootstrap my efforts in the future. Also, while it would be trivial to convert the GVGAI library from JVM to .NET using the IKVM tool, what is not trivial is to figure out that monster library.

I am definitely aiming to exceed the original in simplicity of use.

To use the library just run VGDLFunGen.fsx.

UPDATE 2/28/2016: Chase, Frogs, Missile Command, Portals are done. Will take another break to learn [Idris](http://www.idris-lang.org/) for a while. After this I do not foresee having to take any further detours, but this one might take longer than the IL 2.0 Assembler book. It depends how fun it turns out to be.

UPDATE 2/29/2016: A short review of Idris after a crazy day of focusing full out on it: it is not nearly ready for prime time. The question of whether it is worth seriously using over F# or some other mainstream language is not worth asking at the moment. A more interesting question would be to ask whether a future completed Idris would be a match for the big players. I can't tell at all at this point. Dependent types have a cost in that they weaken the type inference and they blur the distinction between objects and types so there is a price to be paid for them. I am concerned that they make the language less easy to reason about. My programming experience was not effective at all on helping me get a handle on the language due to the blurred boundaries. The documentation is insufficient at this point anyway. The book on Idris by its creator should be a vital resource when it comes out this summer. I'll definitely be interested in reading it.

When I think about languages, I find that unless they are poorly designed, that most of them have their own niche where they are the strongest. Even though I have a high opinion of F#, I can think of things where C++ is better than it for example. I'll refocus on my own niche and get rid of my math envy for now.

I am satisfied with my brief exploration, I'll leave studying depently typed languages on the backburner for a few years - at the moment I'd rather have local references for .NET struct types than dependent types magic. The bleeding edge of language research is definitely not the point of most productivity. I definitely want to finish this library by the end of March. Originally I wanted to do it by the end of this month. I look forward to doing real reinforcement learning after that.

UPDATE 3/1/2016: Done with the first cour. "aliens", "boulderdash", "butterflies", "chase", "frogs", "missilecommand", "portals", "sokoban", "survivezombies", "zelda" are done.

UPDATE 3/3/2016: Updated the disclaimer.

With the expection of "pacman" the "camelRace", "digdug", "firestorms", "infection", "firecaster","overload", , "seaquest", "whackamole", "eggomania" are done now. Finishing Pacman will be less trivial than the others as I have to add AI pathfinding to it. I guess I'll pause a bit to do some research for that.

UPDATE 3/22/2016: Done with pathfinding using [Fringe Search](https://en.wikipedia.org/wiki/Fringe_search). PathChaser has been officially added. It took quite a decent bit [of doing](https://github.com/mrakgr/Pathfinding-Experiments), [not](https://www.hackerrank.com/domains/ai/astar-search) [to](http://theory.stanford.edu/~amitp/GameProgramming/) [mention](https://www.coursera.org/course/algs4partI) [studying](https://www.coursera.org/course/algs4partII), but I finally pulled it off. Cour two is done. Onwards to cour three. I am looking forward to finishing this project.

 UPDATE 3/23/2016:

 Done with the very first version of the library.

Training Set 1 (2015; CIG 2014)
games = new String[]{"aliens", "boulderdash", "butterflies", "chase", "frogs",
        "missilecommand", "portals", "sokoban", "survivezombies", "zelda"};

Training Set 2 (2015; Validation CIG 2014)
games = new String[]{"camelRace", "digdug", "firestorms", "infection", "firecaster",
      "overload", "pacman", "seaquest", "whackamole", "eggomania"};

Training Set 3 (2015)
games = new String[]{"bait", "boloadventures", "brainman", "chipschallenge",  "modality",
                                "painter", "realportals", "realsokoban", "thecitadel", "zenpuzzle"};

Out of the above games "painter", "realportals", "thecitadel", "zenpuzzle" do not work as I haven't stuck to the GVGAI semantics close enough and hit a dead end in the development. Out of the four, Painter works with a hack that I figured out by accident.

On the up-side Pacman has pathfinding as can be seen by running this script.

The reason why I underwent this project is so I can have a runway for my neural net experiments and at the present time, I think this library is good enough for that purpose. Given how long I've been working on this (two whole months non-stop), I had wanted to do much more and maybe later I shall. Given that I've hit a block and I cannot progress without redoing a good piece of the VGDLFunGen.fsx file to more closely conform to the working of the original GVGAI library, it seems like a decent time to pause here.

 Not to mention, it would be embarrasing to continue doing this for longer given that I've yet to try neural nets on pretty much anything.

Enough of this I say, I've proven my spirit enough. I've long wanted to transition to doing actual reinforcement learning and it is time to do so. And with the above selection, I definitely have a diverse set of games to chose from, definitely good enough for an absolute beginner such as myself.

To those of you wandering around this repository with love of Fsharp, machine learning and functional programming in general, you are welcome to the code here for your own endeavors. The Java library is more advanced and has a bunch of neat MCTS controllers in it, but as for this one, you might be able to figure what it does just by reading it.

Dependencies:

* MonoGame
* Fparsec

Disclaimer: The sprites belong to [GVGAI](http://www.gvgai.net/). Also this library not endorsed or supported by Essex University or the original authors. It is solely based on my own initiative and we are in no way related. This port is made for my own learning purposes. Also it is a complete rebuild and it is not based on the Java source so there will be differences even though I've strived to keep the semantics the same.

License: LGPL 3.

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

UPDATE 2/28/2016: Chase, Frogs, Missile Command, Portals are done. Will take another break to learn [Idriss](http://www.idris-lang.org/) for a while. After this I do not foresee having to take any further detours, but this one might take longer than the IL 2.0 Assembler book. It depends how fun it turns out to be.

UPDATE 2/29/2016: A short review of Idris after a crazy day of focusing full out on it: it is not nearly ready for prime time. The question of whether it is worth seriously using over F# or some other mainstream language is not worth asking at the moment. A more interesting question would be to ask whether a future completed Idris would be a match for the big players. I can't tell at all at this point. Dependent types have a cost in that they weaken the type inference and they blur the distinction between objects and types so there is a price to be paid for them. I am concerned that they make the language less easy to reason about. My programming experience was not effective at all on helping me get a handle on the language due to the blurred boundaries. The documentation is insufficient at this point anyway. The book on Idris by its creator should be a vital resource when it comes out this summer. I'll definitely be interested in reading it.

When I think about languages, I find that unless they are poorly designed, that most of them have their own niche where they are the strongest. Even though I have a high opinion of F#, I can think of things where C++ is better than it for example. I'll refocus on my own niche and get rid of my math envy for now.

I am satisfied with my brief exploration, I'll leave studying depently typed languages on the backburner for a few years - at the moment I'd rather have local references for .NET struct types than dependent types magic. The bleeding edge of language research is definitely not the point of most productivity. I definitely want to finish this library by the end of March. Originally I wanted to do it by the end of this month. I look forward to doing real reinforcement learning after that.

UPDATE 3/1/2016: Done with the first cour. "aliens", "boulderdash", "butterflies", "chase", "frogs", "missilecommand", "portals", "sokoban", "survivezombies", "zelda" are done.

Dependencies:

* MonoGame
* Fparsec

Disclaimer: The sprites belong to [GVGAI](http://www.gvgai.net/).
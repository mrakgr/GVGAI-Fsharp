The very first version of the GVGAI-Fsharp.

Aliens, Boulder Dash and the Butterfly games work well currently. Controllers, NPC AI and significant amout of effects are missing for the time being.

I had hope that I would be done by the middle of February, but now on the 20th I see that this is going to take me a while longer. I'll definitely have it all done by the end of March.

Right now, I need to take a break from this in order to learn some .NET assembly in order to better understand F#'s performance. I'll resume work on this before the end of the month.

So far during this project my development has mirrored that of what I had done during the Cool compiler course last year, so it only makes sense to learn some assembly at this stage.

I only decided to do that thing on a whim and never really expected that I would ever write a compiler again.

I had to learn parsing all over again in F#, but I am rather satisfied how things are going so far. Doing this made me realize that I got it all wrong what games were about. Nearly 1k LOC in this project are getting the data into the correct form so the library can use it internally. It is remarkable. So far I am really satisfied how it turned out in terms of design. The more I learn about functional programming the more I love it, it seems.

My goal for this once I am done is to write some proper tutorials regarding the GVGAI-Fsharp library's architecture so that other people can use it and show how the Spiral AD library can be used to enable learning-based controllers. I say 'proper' because my tutorials on the Spiral library so far are clearly no good - literally the only person I can imagine them being useful would be me from a year ago and that is a very limited audience.

I really should be gearing up poker bots instead of doing Atari remakes, but I need a platform where I can easily diagnose the capabilities of the agents I am making. I will definitely need what I am making now to bootstrap my efforts in the future. Also, while it would be trivial to convert the GVGAI library from JVM to .NET using the IKVM tool, what is not trivial is to figure out that monster library.

I am definitely aiming to exceed the original in simplicity of use.

To use the library just run VGDLFunGen.fsx.

Dependencies:

* MonoGame
* Fparsec
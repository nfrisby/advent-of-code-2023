Nicolas Frisby's solutions to [Advent of Code 2023](https://adventofcode.com/2023/about).

As of writing, I've solved all the puzzles already out---that's 1 through 10.
So far, once my code becomes correct for the example, it has been correct on the input.
(That is, no failed submissions :raised_hands:.)
I consider that a compliment to Wastl's specifications and especially choice of included examples.
Moreover, I hope that it's also indicative that I'm doing a good job translating the specification to an "appropriate" implementation, avoiding kludges, etc.

## Disclaimers

The AoC requests that I don't share my puzzle input data nor the puzzle text.
That text includes the example inputs, so I've excluded those as well.

This code is by no means suitable for production!
In particular, I'm considering it merely a vehicle to get the right answer to satisfy the AoC textbox.
I'm not trying to make it maintainable.
So: it should be better commented, it could often be clearer, etc.

I am, on the other hand, trying to keep it relatively simple, avoiding external tools/libraries/etc.
Essentially: you can look at this code if you're curious see how I attack toy-ish problems when avoiding pre-packaged tools and _hurrying_ :grin:.

## The ones that were hard for me so far

Day 5 puzzle 2 and and Day 10 puzzle 2 have taken me the longest, by far.
Multiple hours on Day 5, and two or three on Day 10.

In both cases, the high-level explanation is: I chased my first thought for a while, which turned out to be much more complicated than necessary.
Two reasons: I have a pretty high tolerance for complexity and I've so far been interested in chasing my first thought.
It'll be interesting to assess the consequences once I'm done with the whole thing :grin:.

For Day 5 puzzle 2, I at first tried to compose maps.
I never figured that out, but then I realized one required ingredient was simply applying a map to an interval.
Then I realized that's all I needed to do.
Long-term goal: get more conformation with interval arithmetic.
EG two non-empty intervals can "align" in 13 different ways by my count!
(I'm assuming that avoiding non-empty intervals requires special cases for exact alignment of endpoints.)
This seems too complex for such a simple concept, so I suspect I'm lacking some fundamental conceptual tool.

For Day 10 puzzle 2, I at first tried to cast rays both vertically and horizontally, only counting coordinates that were odd in both.
I eventually realized either one was sufficient by itself.
I blame my confusion on my initially incorrect intution about to handle the fact that in the quantized plane some coordates are _part_ of the pipe.
I still have a suspicion that you could decompose the analysis that way in order to avoid having to track how exactly the loop turns when you're moving through it.

## Caveat emptor

Beyond what I said above in the Disclaimers, there's a few things I'm certain are sloppier than need be.
Here's the list.

I'm _loosely_ trying to reuse as much of puzzle 1 for puzzle 2 each day.
I'm tolerating some hoopjumping for the sake of that.

I haven't written a parser in _ages_.
One thing I haven't bothered to investigate is how to get `megaparsec` to implicitly handle whitespace.
So I've been doing it manually and that's quite ugly.
But tenable for these small parsers.
(Also---in hindsight while writing this README---I should have used the `ReadS` parser to avoid a package dependency.)

There's a few places where I've been excessively monomorphic just to keep it simple.
But, I had to resist a temptation to add in extra polymorphism.
My current through process is that makes this code easier to read, even though my maintainability-based temptation is to trade legibility for precision in the types.

I've also avoided arrays despite them being appealing in Day 10, for example.
So far, the input scale hasn't pushed me over the threshold necessary to incur another dependency.
Also, I'm so far suspecting that each puzzle will admit an algorithm that doesn't require efficient data structures, so I'd prefer to demonstrate that if so.

## My entry for ICFP Contest 2015

I logged just under 25 hours of active work on this code, which
reached 50th on the leaderboard when I retired, about 50 hours into
the contest. 22 hours of inactivity later, I was at 113th of the 194
teams which managed to submit scoring plays.

The organisation of the ~450 lines of Haskell code is a bit haphazard,
with some misleading comments. If in doubt, follow the code down from
`Main`.

The gameplay logic is in `GamePlay.hs`, where `playCommand` can be
given a `Command` to advance one `GameState` to a new `GameState`.

The code in `AI` uses `playCommand` to explore paths taking the
current unit to all the possible locked positions, then chooses the
"best" position from which to move on to placing the next unit. "Best"
is a heuristic based on the score for that move, plus a bonus for
moves which result in more filled cells lower on the grid. Simple, but
fairly effective.

Haskell was a great choice for this challenge, because the time I
spent scratching my head was more than compensated for by the ease of
expressing algorithms, and the guarantee of immutability.

In due course I hope to write a full post-mortem of what was a
wonderful - if exhausting - experience.

-Steve Purcell

[@sanityinc](https://twitter.com/sanityinc) // [sanityinc.com](http://www.sanityinc.com)

Get in touch if you'd like to work with me at
[powershop.co.nz](http://www.powershop.co.nz/) -- we're hiring
excellent developers.

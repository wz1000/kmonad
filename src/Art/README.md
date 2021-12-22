The Ctx module is meant for:

Little bits of utility that add some functionality to the monad they are
included in. These utilities should not need to know about the larger whole,
should be easily portable, and only need local type-info.

At the time of writing, this includes:
- logging
- handling locale dictionaries
- handling external commands

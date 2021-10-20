dev-note:

I tried to put the `System.Keyboard` module in its own internal library, and
although this is supported and works well for cabal, it gives stack a headache.
`stack build` works, but `stack repl` gets fits, and `stack repl` is probably
how most people want to dev.

There are open issues about this on the `stack` github. 


So, for now, I put this into the basic app-project in KMonad, but please,
respect the following rules:
- `System.Keyboard` may *NOT* import from `KMonad.`
- `KMonad` may *ONLY* import `System.Keyboard` and `System.Keyboard.IO`.

Once the keycode refactor is running and tested, I'll invest the effort into
actually making `System.Keyboard` its own proper project, and figuring out how
to import it directly from git into cabal (so as to bypass hackage time-delays)
while also making an effort to get it documented and on Hackage.

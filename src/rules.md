# Rules that code should follow

Configurations should always be (Show,Eq)able

in a `Types.hs` + {`Types/Linux.hs`, `Types/Mac.hs`} type situation, the OS specific ones may depend on the `Types.hs` but not vice versa.


A simple Scheme compiler written in Haskell.

## TESTING:
On the commanline, first move to the directory where the hsScheme.cabal file is. Then

cabal install --prefix=$HOME --user

Installs the programme in $HOME/bin/

Run with:
hsScheme <flags>


## Supported flags
* _-t_: Run the tests.
* _-c_: Compile the string given in the second argument.
* _-g_: Show the string given in the second argument in a tree like(flat)
representation.

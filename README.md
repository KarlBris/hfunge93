# hfunge93

A Befunge-93 interpreter written in Haskell


## Usage

    $ ghci
    Prelude> :l Befunge93Interpreter.hs
    [1 of 3] Compiling Befunge93Data    ( Befunge93Data.hs, interpreted )
    [2 of 3] Compiling Befunge93Logic   ( Befunge93Logic.hs, interpreted )
    [3 of 3] Compiling Befunge93Interpreter ( Befunge93Interpreter.hs, interpreted )
    Ok, three modules loaded.
    *Befunge93Interpreter> interpret ".\\hello.bf"
    Hello World!


## Example code

For example funges, take a look at the [example programs directory in catseye/Befunge-93](https://github.com/catseye/Befunge-93/tree/master/eg)
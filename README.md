This bot connects an Idris REPL with IRC by running `idris` in IDEslave mode,
piping queries from IRC to it, interpreting the responses and sending them
back.

It runs `idris` under `sandbox` with the package directories provided within.
If given an idris file as an argument, it will copy this into the sandbox as
well and pass it to `idris` as an argument, allowing one to have more than the
Prelude available by putting import lines into this file.

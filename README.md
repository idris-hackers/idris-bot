This bot connects an Idris REPL with IRC by running `idris` in IDEslave mode,
piping queries from IRC to it, interpreting the responses and sending them
back.

It runs `idris` under `sandbox` with the `base` library directory provided
within.

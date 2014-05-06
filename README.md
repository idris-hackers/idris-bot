This bot connects an Idris REPL with IRC by running `idris` in IDEslave mode,
piping queries from IRC to it, interpreting the responses and sending them
back.

One argument is required, naming the config file to use.

`idris` is run under `sandbox` with the package directories provided within. If
given an idris file as second argument, it will copy this into the sandbox as
well and pass it to `idris` as an argument, allowing one to have more than the
Prelude available by putting import lines into this file.


### Configuration

Global options must be in no section or in section `[DEFAULT]` (if they appear
elsewhere, those have no effect). A section `[#example]` will configure options
for the channel `#example`, with defaults from the default section or the
internal defaults (i.e. to configure defaults for all channels, place them into
section `[DEFAULT]`). `[noChannel]` configures options for direct messages.

#### Options

- `network`: (bare word, global) Hostname to connect to. Required.
- `nick`: (bare word, global) Nick to use. Default: `idris-ircslave`
- `channels`: (`[String]`, global) Channels to join. Default: `[]`
- `maxCharsPerLine`: (`Maybe Int`) Character limit per line. `Nothing` means
  unlimited. Default: `Just 400`
- `maxLinesPerResponse`: (`Maybe Int`) Line limit per response. `Nothing` means
  unlimited. Default: `Just 5`
- `consoleWidth`: (`Maybe Int`, global) Width for line wrapping on idris’ side.
  `Nothing` means idris’ default. Default: `Just 200`
- `allowedCommands`: (`[String]`) Whitelist for “:”-commands. A command not on
  this list is responded to with an error message. Default: `["t", "type"]`
- `interpPrefixes`: (`[String]`) List of prefixes to trigger interpretation on,
  i.e. lines beginning with one of these will be considered requests. Default:
  `["> "]`

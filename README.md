# p2psim

p2psim is a p2p network simulator.

## Usage 
run this program by specifying the config file as an argument: 
```sh
$ p2psim config.toml 
```

you can take a look at how this config file may look by looking at
[example.toml](examples/example.toml).

using the "help" command at the repl will show help for all available commmands.

## Build
### Requirements:
1. install GHC and Cabal using [GHCup](https://www.haskell.org/ghcup/).
2. install [graphviz](https://www.graphviz.org) too if you want to render your graph.

### Compiling:
#### Running:
```sh
$ cabal run p2psim.cabal -- your_config.toml
```

#### Installing:
this will install the application as a binary named "p2psim" inside your cabal
directory.
```sh
$ cabal install
```

# Fair Refinement for Asynchronous Session Types

This is an implementation of the algorithm given in the paper *Fair
Refinement for Asynchronous Session Types*  by Mario Bravetti,  Julien
Lange, and Gianluigi Zavattaro.

## Requirements:

You will need [ghc](https://www.haskell.org/platform/) and [GraphViz (dot)](https://www.graphviz.org/) installed on your machine (the last one is only required to generate graphs).

Depending on your setup, you might need to install extra Haskell
packages (GHC's complaints will help you figure these out and you can
install them with `cabal`). For instance, you will need `cmdargs` and `parsec`
which you can install with `cabal install cmdargs parsec --lib`.


## Compiling and running the tool

Here, we assume that you have a terminal open and that you are in the `root` sub-directory.

### Compile:

* Run: `ghc Checker`

### Usage

There are two modes, in the "passive" mode you give the input parameters as file paths, in the "interactive" mode, you give them directly as strings.

#### Passive mode

* Run the following to check whether T1 is a fair asynchronous subtype of T2  `./Checker <path-to-file-T1> <path-to-file-T2>`

* Add the `--debug` flag if you want to print graphs (this might take a while in some cases)

The files `file-Ti` must contain session types of the following shape:

- `!a;?b;end`
- `rec X . !a; [?d;X, ?x;!b;X]`
- `rec X. [!a;[?x;X, ?d;X], !b;[?x;X, ?d;X]]`


where, e.g., `rec X. T` is a recursion definition, `X` is a recursive call, `[!a;T1, !b;T2]` is an internal choice, `[?a;T1, ?b;T2]` is an external choice, and `end` is termination. You can omit the square brackets for unary choice (one send/receive action).

#### Interactive mode

* Run the following to check whether T1 is a fair asynchronous subtype of T2  `./Checker -iT 'T1' 'T2'`

where T1 and T2 are session types, e.g.,

* `./Checker -i '!a;?b;end' '?b;!a;end'` (add the `--debug` flag to generate graphs)


In both modes, the tool says *True* if T1 is a subtype of T2, *False* if not, and *Maybe* if the tool cannot give a definitive answer.

When you set the `--debug` flag, the tool generates the following files in the current directory:

* `sub_cfsm.svg` is T1 and `sup_cfsm.svg` is T2 (graphical representation thereof)
* `sim_tree.png` is the (partial) simulation tree 
* `witness_trees.png` is the list of candidate subtrees.

It will also print something like:

```
Candidate sub-type controllable: True
Candidate super-type controllable: True
Candidate sub-type strong controllable: True
Candidate super-type strong controllable: True
```
which says whether the input types are (strong) controllable.


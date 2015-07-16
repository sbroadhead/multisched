# multisched
Masters degree work repo/depot.
[![Build Status](https://travis-ci.org/sbroadhead/multisched.svg?branch=master)](https://travis-ci.org/sbroadhead/multisched)

## Usage
Currently the frontend is simply a launcher for demos (essentially interactive unit tests). Eventually it will house any scheduling and simulation tools that are built. Build the frontend using `sbt`, or use the `sbt run` command to run it directly. Use `--list-demos` to list the available demos that are available, and `--demo DemoName [args...]` to invoke a demo.

## Progress
Got the CodeGraph builder working and imported the SPU version of Exp from the Haskell version.
<img src="https://cdn.rawgit.com/sbroadhead/multisched/master/misc/images/expuneval.svg" width="100%">

Also got evaluation of the CodeGraph working, and the exp function is now working. Also got cool annotated dot graph
output working.
<img src="https://cdn.rawgit.com/sbroadhead/multisched/master/misc/images/expeval.svg" width="100%">

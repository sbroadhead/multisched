# multisched
Masters degree work repo/depot.
[![Build Status](https://travis-ci.org/sbroadhead/multisched.svg?branch=master)](https://travis-ci.org/sbroadhead/multisched)

## Usage
Currently the frontend is simply a launcher for demos (essentially interactive unit tests). Eventually it will house any scheduling and simulation tools that are built. Build the frontend using `sbt`, or use the `sbt run` command to run it directly. Use `--list-demos` to list the available demos that are available, and `--demo DemoName [args...]` to invoke a demo.

## Progress
Got the CodeGraph builder working and imported the SPU version of Exp from the Haskell version.
<img src="https://raw.githubusercontent.com/sbroadhead/multisched/master/misc/images/expgraph.png" width="100%">

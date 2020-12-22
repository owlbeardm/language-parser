#!/bin/bash

stack bench --ba "--output docs/bench.html";
stack haddock --haddock-arguments "--odir=docs";
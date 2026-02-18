#!/bin/bash
cat preludeCaml.ml $1 > /tmp/runcaml.ml; ocaml /tmp/runcaml.ml


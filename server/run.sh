#!/usr/bin/env bash
ocamlc -g str.cma unix.cma server.ml
./a.out localhost 4000

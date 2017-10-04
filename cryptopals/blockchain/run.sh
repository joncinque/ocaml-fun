#!/usr/bin/env bash
ocamlc -g -I ../helper ../helper/convert.ml ../helper/hash.ml blockchain.ml
./a.out

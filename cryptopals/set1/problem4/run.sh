#!/usr/bin/env bash
ocamlc -g -o problem4 -I ../../helper ../../helper/convert.ml ../../helper/xor_score.ml problem4.ml
./problem4

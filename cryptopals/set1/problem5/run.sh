#!/usr/bin/env bash
ocamlc -g -o problem5 -I ../../helper ../../helper/convert.ml ../../helper/xor_score.ml problem5.ml
./problem5

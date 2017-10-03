#!/usr/bin/env bash
ocamlc -g -o problem3 -I ../../helper ../../helper/convert.ml ../../helper/xor_score.ml problem3.ml
./problem3

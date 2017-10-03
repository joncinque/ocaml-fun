#!/usr/bin/env bash
ocamlc -g -o problem2 -I ../../helper nums.cma ../../helper/convert.ml problem2.ml
./problem2

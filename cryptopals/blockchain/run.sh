#!/usr/bin/env bash
ocamlc -g -o blockchain -I ../helper ../helper/convert.ml blockchain.ml
./blockchain

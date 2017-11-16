#!/usr/bin/env bash
stack exec cardano-auxx -- cmd --commands "hash-installer $1" --mode=light

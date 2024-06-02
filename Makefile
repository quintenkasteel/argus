ROOT = $(dir $(realpath $(firstword $(MAKEFILE_LIST))))
SHELL := /bin/bash

.PHONY: $(MAKECMDGOALS)

default: help

help:
	@echo "assets: Add assets locally"


build:
	@stack build --fast --pedantic --ghc-options "-j +RTS -A128m -n2m -RTS" --copy-bins

build-watch:
	@stack install --file-watch --fast --pedantic --ghc-options "-j +RTS -A128m -n2m -RTS"

run:
	@code-conventions data --in-place --improve
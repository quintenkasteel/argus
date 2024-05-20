ROOT = $(dir $(realpath $(firstword $(MAKEFILE_LIST))))
SHELL := /bin/bash

.PHONY: $(MAKECMDGOALS)

default: help

help:
	@echo "assets: Add assets locally"


build:
	@stack install

run:
	@linter
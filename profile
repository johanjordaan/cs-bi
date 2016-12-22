#!/bin/bash
stack build --executable-profiling --library-profiling
stack exec -- cs-bi-exe +RTS -p

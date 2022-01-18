#!/bin/bash

ARGS=($*)

echo $((${#ARGS[@]} - 1))

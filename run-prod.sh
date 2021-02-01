#!/bin/bash

source ./prod-export.sh

echo $PGHOST

export ENV=Production

stack run
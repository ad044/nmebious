#!/bin/bash

set -xe

while !</dev/tcp/db/5432;
  do sleep 1;
done;

make && ./nmebious

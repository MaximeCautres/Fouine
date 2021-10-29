#!/bin/sh
for f in tests/*/*.ml; do echo "$f"; ./fouine -cps -autotest $f; done | grep -v OK


#!/bin/bash
set -e 
echo "received: $(awk -f main.awk example.txt), expected: 114";
echo "received: $(awk -f main.awk input.txt), expected: 1772145754";

echo "received: $(awk -v part=2 -f main.awk input.txt), expected: 867";

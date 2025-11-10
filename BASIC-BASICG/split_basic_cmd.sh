#!/bin/bash

BASIC=BASIC
SIZE=$(stat -c%s ${BASIC}_FULL.CMD)

echo "--- Input file size = $SIZE ---"

echo "=== Building BASIC.OV1 ==="
echo ". Executable code starting from address 2600H"
dd if=${BASIC}_FULL.CMD of=${BASIC}.OV1 bs=1 count=2600
echo ". Transfer record type & len (02H,02H)"
dd if=${BASIC}_FULL.CMD of=${BASIC}.OV1 bs=1 skip=$(($SIZE-4)) seek=2600 count=2
echo ". Transfer record start address from first code record address 2600H"
dd if=${BASIC}_FULL.CMD of=${BASIC}.OV1 bs=1 skip=2 seek=2602 count=2

echo "=== Building BASIC.CMD ==="
echo ". Executable code starting from address 3000H + transfer record type & len (02H,02H)"
dd if=${BASIC}_FULL.CMD of=${BASIC}.CMD bs=1 skip=2600 count=$(($SIZE-2600-2))
echo ". Transfer record start address from initial JP instruction at address 2600H+1"
dd if=${BASIC}_FULL.CMD of=${BASIC}.CMD bs=1 skip=5 seek=$(($SIZE-2600-2)) count=2

read -p "Press ENTER:"

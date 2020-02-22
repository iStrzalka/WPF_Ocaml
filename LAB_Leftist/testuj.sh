#!/bin/bash

cd "$(dirname "$0")"

echo Kompiluję Drzewa Lewicowe

ocamlc -c leftist.mli leftist.ml

for f in tests/*.ml
do
    echo Przetwarzam: $(basename "$f")
    time (ocamlc -c "$f"
    ocamlc -o "${f%%.*}" leftist.cmo "${f%%.*}".cmo
    ./"${f%%.*}"
    rm "${f%%.*}" "${f%%.*}".cmo "${f%%.*}".cmi)
done

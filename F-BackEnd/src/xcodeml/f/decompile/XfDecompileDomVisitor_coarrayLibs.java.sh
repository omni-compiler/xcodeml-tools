#!/bin/bash

CallingClassName="XfDecompileDomVisitor"
CallingFile=${CallingClassName}.java
ClassName=${CallingClassName}_coarrayLibs

ifile="$1"
INCDIR="$2"
#ifile=${OMNI_HOME}/omni-compiler/libxmpf/src/xmpf_coarray_decl.f90
#INCDIR=${OMNI_HOME}/omni-compiler/libxmpf/include

if ! test -r "${ifile}"; then
    echo "$0: unreadable input file: \'${ifile}\'"
    exit 1
fi

if ! test -d "${INCDIR}"; then
    echo "$0: illegal include directory: \'${INCDIR}\'"
    exit 1
fi


function output_entry_names {
    awk '
        $1 ~ /^interface|function|subroutine$/ &&
        $2 ~ /^[a-z][a-z0-9_][a-z0-9_][a-z0-9_]/ {
            split($2, token, "(")
            printf "        \"%s\",\n", token[1]
        }
        $1 != "end" && $1 !~ /^!/ &&
        $2 ~ /^interface|function|subroutine$/ &&
        $3 ~ /^[a-z][a-z0-9_][a-z0-9_][a-z0-9_]/ {
            split($3, token, "(")
            printf "        \"%s\",\n", token[1]
        }
    ' $1
}

function get_include_files {
    awk '
        $1 ~ /^include$/ &&
        $2 ~ /^"[a-zA-Z0-9_.]+"$/ {
            split($2, token, "\"")
            print token[2]
        }
    ' $1
}


function output_header {
    echo '
// This file was automatically generated by
//    '$1'
// to be read by
//    '$CallingFile'
//

package xcodeml.f.decompile;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Arrays;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.io.*;

public class '$2'
{
    public final static String[] EntryNameArray = {'   # end of echo
}

function output_tailer {
    echo '
        ""
    };
}
' # end of echo
}


## output header lines
output_header $0 $ClassName

## output library names in $ifile
output_entry_names $ifile

## get include file names in $ifile
incfiles=`get_include_files $ifile`

## output interface/subroutine/function names in all include files
for f in ${incfiles}; do
    output_entry_names ${INCDIR}/$f
done

## output tailer lines
output_tailer

exit

#!/bin/bash

# language specification is required
CMD=`basename $0`
DIR=`dirname $0`
USAGE="usage: $CMD [-h] [-v] [-p] [-r] [-c] -l language_name [-k [fixed_grammar_file]]"
if [ X"$1" == X"" ]; then
  echo ${USAGE}
  exit 1
fi

export LANG_NAME="dummy"
export GIVEN_FIX_GRAMMAR_FILE="default"
export RESUME=0
export PARSER_GEN=0
export RUNNER_GEN=0
export REMOVE_FILES=0
export VERBOSE_MODE=0

while getopts ":hvl:prck:" opt; do
  case $opt in
    h) 
      echo ${USAGE}
      echo "  -h : help"
      echo "  -v : verbose mode"
      echo "  -l language_name : specify language name"
      echo "       (language_name-lang.rkt should exist)"
      echo "  -p : generate parser"
      echo "  -r : generate runner"
      echo "  -c : clean up generated files (no other processing performed)"
      echo "  -k fixed_grammar_file : specify surface grammar file"
      echo "       (grammar extracting process is not invoked; fixed_grammar_file should exist)"
      echo "       clean up generated files (no processing performed)"
      exit 1
      ;;
    v)
      echo "verbose mode"
      export VERBOSE_MODE=1
      ;;
    l)
      echo "LANG_NAME=$OPTARG"
      export LANG_NAME=$OPTARG
      ;;
    p)
      export PARSER_GEN=1
      ;;
    r)
      export RUNNER_GEN=1
      ;;
    c)
      export REMOVE_FILES=1
      ;;
    k)
      if [ X"$OPTARG" == X"" ]; then
        export GIVEN_FIX_GRAMMAR_FILE="default"
      else
        echo "GIVEN_FIX_GRAMMAR_FILE=$OPTARG"
        export GIVEN_FIX_GRAMMAR_FILE=$OPTARG
      fi
      export RESUME=1
      ;;
    \?)
        echo "invalid option: -$OPTARG" >&2
        exit 1
        ;;
    :)
        if [ $OPTARG != "k" ]; then
          echo "option -$OPTARG requires an argument" >&2
          exit 1
        fi
        if [ $OPTARG == "k" ]; then
          export RESUME=1
        fi
        ;;
  esac
done

echo $LANG_FILE

if [ X"$LANG_NAME" == X"dummy" ]; then
  echo "Error: Language specification needed."
  exit 1
fi

if [ $VERBOSE_MODE -eq 1 ]; then
  set -x
fi

# Directories
LIBDIR="${DIR}/../lib"
PWD=`pwd`
BUILDDIR="${PWD}/build"
if [ ! -d $BUILDDIR ]; then
  mkdir $BUILDDIR
fi

##################
# System files
EXTRACT_SYNTAX="${LIBDIR}/extract-syntax.rkt"
MERGE_SYNTAX="${LIBDIR}/merge-syntax.rkt"
MAKE_GRAMMAR="${LIBDIR}/make-grammar.rkt"
MAKE_AST2LIST="${LIBDIR}/make-ast-to-list.rkt"
CHECK_GRAMMAR="${LIBDIR}/make-check.rkt"
GENERIC_INTERP="${LIBDIR}/interp.rkt"
GENERIC_PARSER="${LIBDIR}/parser.rkt"
GENERIC_RUNNER="${LIBDIR}/runner.rkt"

# note that the following files are also used
# ${LIBDIR}/common.rkt
# ${LIBDIR}/parser0.rkt
# ${LIBDIR}/runner0.rkt

##################
# Input file
LANG_FILE="${LANG_NAME}-lang.rkt"

##################
# Files to be generated
SYNTAX_FILE="${BUILDDIR}/$LANG_NAME-lang-syntax.rkt"
MERGED_FILE="${BUILDDIR}/$LANG_NAME-merged-lang.rkt"
SYMBOLS_FILE="${BUILDDIR}/$LANG_NAME-start-symbol.rkt"
GRAMMAR_FILE="${BUILDDIR}/$LANG_NAME-eopl-grammar.rkt"
FIX_GRAMMAR_FILE_NAME="$LANG_NAME-fix-eopl-grammar.rkt"
if [ X"$GIVEN_FIX_GRAMMAR_FILE" != X"default" ]; then
  FIX_GRAMMAR_FILE="$GIVEN_FIX_GRAMMAR_FILE"
else
  FIX_GRAMMAR_FILE="${BUILDDIR}/${FIX_GRAMMAR_FILE_NAME}"
fi
AST2LIST_FILE="${BUILDDIR}/$LANG_NAME-ast-to-list.rkt"
CHECK_FILE="${BUILDDIR}/$LANG_NAME-check.sh"
INTERP_NAME="$LANG_NAME-interp.sh"
INTERP="${BUILDDIR}/${INTERP_NAME}"

PARSER_NAME="$LANG_NAME-parser.sh"
PARSER="${BUILDDIR}/${PARSER_NAME}"
RUNNER_NAME="$LANG_NAME-runner.sh"
RUNNER="${BUILDDIR}/${RUNNER_NAME}"


##################
# Clean up files and exit
if [ ${REMOVE_FILES} -eq 1 ]; then
  echo "Removing following files (if exits):"  
  echo "${SYNTAX_FILE}"
  echo "${MERGED_FILE}"
  echo "${SYMBOLS_FILE}"
  echo "${GRAMMAR_FILE}"
  echo "${FIX_GRAMMAR_FILE}"
  echo "${AST2LIST_FILE}"
  echo "${CHECK_FILE}"
  echo "${INTERP}"
  echo "${PARSER}"
  echo "${RUNNER}"
  echo "./${INTERP_NAME}"
  echo "./${PARSER_NAME}"
  echo "./${RUNNER_NAME}"
  read -p "OK? [y/N]: " response
  response=${response:-n}
  if [[ "$response" =~ ^[Yy]$ ]]; then
    rm -f "${SYNTAX_FILE}"
    rm -f "${MERGED_FILE}"
    rm -f "${SYMBOLS_FILE}"
    rm -f "${GRAMMAR_FILE}"
    rm -f "${FIX_GRAMMAR_FILE}"
    rm -f "${AST2LIST_FILE}"
    rm -f "${CHECK_FILE}"
    rm -f "./${INTERP_NAME}"
    rm -f "${INTERP}"
    rm -f "./${PARSER_NAME}"
    rm -f "${PARSER}"
    rm -f "./${RUNNER_NAME}"
    rm -f "${RUNNER}"
  else
    echo "Nothing has done."
  fi
  exit 1
fi

##################
# Generate files for parser generator

if [ $RESUME -eq 0 ]; then # option -k is not given
  echo "Extracting syntax information from ${LANG_FILE}..."
  racket ${EXTRACT_SYNTAX} ${LANG_FILE} ${SYNTAX_FILE}
  racket ${MERGE_SYNTAX} ${SYNTAX_FILE} ${MERGED_FILE} ${SYMBOLS_FILE}
#  racket ${MAKE_GRAMMAR} ${MERGED_FILE} ${GRAMMAR_FILE} ${SYNTAX_FILE} ${FIX_GRAMMAR_FILE}
  racket ${MAKE_GRAMMAR} ${MERGED_FILE} ${GRAMMAR_FILE} ${SYMBOLS_FILE} ${FIX_GRAMMAR_FILE}
fi
if [ $? -ne 0 ]; then
  echo STOP
  exit 1
fi

##################
# Check grammar

echo "Checking `basename ${FIX_GRAMMAR_FILE}`..."
mv -f ${CHECK_FILE} ${CHECK_FILE}.orig > /dev/null 2>&1
#raco fmt -i ${FIX_GRAMMAR_FILE}
SUBCMD="(enter! (file \"${CHECK_GRAMMAR}\"))"
echo racket --eval \'${SUBCMD}\' ${FIX_GRAMMAR_FILE} > ${CHECK_FILE}
sh ${CHECK_FILE}
if [ $? -eq 0 ]; then
  raco fmt -i ${FIX_GRAMMAR_FILE}
  echo OK
  echo
else
  raco fmt -i ${FIX_GRAMMAR_FILE}
  echo
  echo "Build procces stopped. Please check `basename ${FIX_GRAMMAR_FILE}`."
  echo " 1. Fix it (or a copied one) to make it LL(1) in SLLGEN format."
  echo " 2. Run this script again as follows:"
  echo "  ${CMD} -l ${LANG_NAME} -k [fixed_grammar_file_name]"
  exit 1
fi

echo "Generating `basename ${AST2LIST_FILE}`..."
if [ ${GIVEN_FIX_GRAMMAR_FILE} != "default" ]; then # fixed grammar is given separately
  cp ${GIVEN_FIX_GRAMMAR_FILE} "${BUILDDIR}/${FIX_GRAMMAR_FILE_NAME}"
fi
#racket ${MAKE_AST2LIST} ${GRAMMAR_FILE} ${AST2LIST_FILE} ${FIX_GRAMMAR_FILE_NAME}
racket ${MAKE_AST2LIST} ${GRAMMAR_FILE} ${AST2LIST_FILE} ${FIX_GRAMMAR_FILE_NAME} ${MERGED_FILE}
if [ $? -ne 0 ]; then
  echo STOP
  exit 1
fi

##################
# Output a specific script that works as an interpreter

echo "Generating `basename ${INTERP}`..."
mv -f ${INTERP} ${INTERP}.orig > /dev/null 2>&1
echo "#!/bin/bash" >> ${INTERP}
echo "INPUT_FILE=\$1" >> ${INTERP}
echo racket ${GENERIC_INTERP} ${AST2LIST_FILE} ${LANG_FILE} '${INPUT_FILE}' >> ${INTERP}
chmod +x ${INTERP}
ln -s ${INTERP} ${INTERP_NAME} > /dev/null 2>&1

echo "=========================================="
echo "`basename ${INTERP}` has been build."
echo "  Usage : ./${INTERP_NAME} program.txt"
echo "=========================================="

# standalone parser (source->s-expression)
if [ $PARSER_GEN -eq 1 ]; then
  mv -f ${PARSER} ${PASER}.orig > /dev/null 2>&1
  echo "#!/bin/bash" >> ${PARSER}
  echo "INPUT_FILE=\$1" >> ${PARSER}
  echo racket ${GENERIC_PARSER} ${AST2LIST_FILE} '${INPUT_FILE}' >> ${PARSER}
  chmod +x ${PARSER}
  ln -s ${PARSER} ${PARSER_NAME} > /dev/null 2>&1

  echo "=========================================="
  echo "`basename ${PARSER}` has been build."
  echo "  Usage : ./${PARSER_NAME} program.txt"
  echo "=========================================="
fi

# standalone runner (runs a program given in s-expression)
if [ $RUNNER_GEN -eq 1 ]; then
  mv -f ${RUNNER} ${RUNNER}.orig > /dev/null 2>&1
  echo "#!/bin/bash" >> ${RUNNER}
  echo "INPUT_FILE=\$1" >> ${RUNNER}
  echo racket ${GENERIC_RUNNER} ${LANG_FILE} '${INPUT_FILE}' >> ${RUNNER}
  chmod +x ${RUNNER}
  ln -s ${RUNNER} ${RUNNER_NAME} > /dev/null 2>&1

  echo "=========================================="
  echo "`basename ${RUNNER}` has been build."
  echo "  Usage : ./${RUNNER_NAME} program.sexp"
  echo "=========================================="
fi


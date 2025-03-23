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
#LIBDIR="./lib"
#BUILDDIR="./build"
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
SYMBOLS_FILE="${BUILDDIR}/$LANG_NAME-start-symbol.rkt"
#MAKE_GRAMMAR="${LIBDIR}/make-grammar.rkt"
MAKE_OCAMLLEX="${LIBDIR}/make-ocamllex.rkt"
MAKE_OCAMLYACC="${LIBDIR}/make-ocamlyacc.rkt"
MAKE_SYNTAX="${LIBDIR}/make-syntax.rkt"
CHECK_OCAML="${LIBDIR}/check-ocaml.rkt"
#MAKE_AST2LIST="${LIBDIR}/make-ast-to-list.rkt"
MAKE_AST2LIST="${LIBDIR}/make-ocaml-to-sexpr.rkt"
GENERIC_OCAML_PARSER_NAME="ocaml_parser_main"
GENERIC_OCAML_PARSER="${LIBDIR}/${GENERIC_OCAML_PARSER_NAME}.ml"
#CHECK_GRAMMAR="${LIBDIR}/make-check.rkt"
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
#AST_DATATYPE_FILE="${BUILDDIR}/$LANG_NAME-ast-datatype.ml"
AST_DATATYPE_FILE="${BUILDDIR}/syntax.ml"
#GRAMMAR_FILE="${BUILDDIR}/$LANG_NAME-eopl-grammar.rkt"
#FIX_GRAMMAR_FILE_NAME="$LANG_NAME-fix-eopl-grammar.rkt"
#FIX_GRAMMAR_FILE="${BUILDDIR}/${FIX_GRAMMAR_FILE_NAME}"
#OCAMLLEX_FILE="${BUILDDIR}/${LANG_NAME}_lexer.mll"
OCAMLLEX_FILE="${BUILDDIR}/lexer.mll"
#OCAMLYACC_FILE="${BUILDDIR}/${LANG_NAME}_parser.mly"
OCAMLYACC_FILE="${BUILDDIR}/parser.mly"
#AST2LIST_FILE="${BUILDDIR}/$LANG_NAME-ast-to-list.rkt"
#AST2LIST_FILE="${BUILDDIR}/$LANG_NAME-ast-to-list.ml"
#AST2LIST_FILE="${BUILDDIR}/${LANG_NAME}_ast_to_list.ml"
AST2LIST_FILE="${BUILDDIR}/ast_to_sexpr.ml"
#CHECK_FILE="${BUILDDIR}/$LANG_NAME-check.sh"
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
  echo "${AST_DATATYPE_FILE}"
#  echo "${GRAMMAR_FILE}"
#  echo "${FIX_GRAMMAR_FILE}"
  echo "${AST2LIST_FILE}"
#  echo "${CHECK_FILE}"

  echo ${BUILDDIR}/${GENERIC_OCAML_PARSER_NAME}
  echo ${OCAMLLEX_FILE}
  echo ${OCAMLLEX_FILE%.mll}.cmo
  echo ${OCAMLYACC_FILE}
  echo ${OCAMLYACC_FILE%.mly}.cmo
  echo ${OCAMLYACC_FILE%.mly}.mli
  echo ${AST2LIST_FILE%.ml}.cmo
#  echo ${GENERIC_OCAML_PARSER%.ml}.cmo
  echo ${AST_DATATYPE_FILE%.ml}.cmo
  echo ${OCAMLLEX_FILE%.mll}.cmi
  echo ${OCAMLYACC_FILE%.mly}.cmi
  echo ${AST2LIST_FILE%.ml}.cmi
#  echo ${GENERIC_OCAML_PARSER%.ml}.cmi
  echo ${AST_DATATYPE_FILE%.ml}.cmi
  echo ${OCAMLLEX_FILE%.mll}.ml
  echo ${OCAMLYACC_FILE%.mly}.ml
 
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
    rm -f "${AST_DATATYPE_FILE}"
#    rm -f "${GRAMMAR_FILE}"
#    rm -f "${FIX_GRAMMAR_FILE}"
    rm -f "${AST2LIST_FILE}"
#    rm -f "${CHECK_FILE}"

    rm -f ${BUILDDIR}/${GENERIC_OCAML_PARSER_NAME}
    rm -f ${OCAMLLEX_FILE}
    rm -f ${OCAMLLEX_FILE%.mll}.cmo
    rm -f ${OCAMLYACC_FILE}
    rm -f ${OCAMLYACC_FILE%.mly}.cmo
    rm -f ${OCAMLYACC_FILE%.mly}.mli
    rm -f ${AST2LIST_FILE%.ml}.cmo
#    rm -f ${GENERIC_OCAML_PARSER%.ml}.cmo
    rm -f ${AST_DATATYPE_FILE%.ml}.cmo
    rm -f ${OCAMLLEX_FILE%.mll}.cmi
    rm -f ${OCAMLYACC_FILE%.mly}.cmi
    rm -f ${AST2LIST_FILE%.ml}.cmi
#    rm -f ${GENERIC_OCAML_PARSER%.ml}.cmi
    rm -f ${AST_DATATYPE_FILE%.ml}.cmi
    rm -f ${OCAMLLEX_FILE%.mll}.ml
    rm -f ${OCAMLYACC_FILE%.mly}.ml

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
  racket ${MAKE_OCAMLLEX} ${MERGED_FILE} ${OCAMLLEX_FILE}
  racket ${MAKE_OCAMLYACC} ${MERGED_FILE} ${OCAMLYACC_FILE} ${SYMBOLS_FILE}
  cp ${OCAMLYACC_FILE} ${OCAMLYACC_FILE%.mly}.orig.mly
fi
#racket ${MAKE_SYNTAX} ${MERGED_FILE} ${AST_DATATYPE_FILE} ${SYMBOLS_FILE} ${OCAMLYACC_FILE}
racket ${MAKE_SYNTAX} ${MERGED_FILE} ${AST_DATATYPE_FILE} ${SYMBOLS_FILE} ${OCAMLYACC_FILE%.mly}.orig.mly
if [ $? -ne 0 ]; then
  echo "STOP"
  exit 1
fi

# ##################
# # Check grammar

echo "Checking `basename ${OCAMLYACC_FILE}`..."
#OCAMLYACC_PATH=$(which ocamlyacc)
OCAMLYACC_PATH=$(which bison)

racket ${CHECK_OCAML} ${OCAMLYACC_PATH} ${OCAMLYACC_FILE}
if [ $? -eq 0 ]; then
  echo OK
  echo
else
  echo
  echo "Build procces stopped. Please check ${OCAMLYACC_FILE}."
  echo " 1. Fix it (or a copied one) to make it LALR(1) in OCamlyacc format."
  echo " 2. Run this script again as follows:"
  echo "  ${CMD} -l ${LANG_NAME} -k [fixed_grammar_file_name]"
  exit 1
fi

# TODO

echo "Generating `basename ${AST2LIST_FILE}`..."
#racket ${MAKE_AST2LIST} ${MERGED_FILE} ${OCAMLYACC_FILE} ${AST_DATATYPE_FILE} ${AST2LIST_FILE}
racket ${MAKE_AST2LIST} ${MERGED_FILE} ${OCAMLYACC_FILE%.mly}.orig.mly ${AST_DATATYPE_FILE} ${AST2LIST_FILE}
if [ $? -ne 0 ]; then
  echo "STOP"
  exit 1
fi

AST_DATATYPE_FILE_MLI="${AST_DATATYPE_FILE%.ml}.mli"
OCAMLYACC_FILE_MLI=${OCAMLYACC_FILE%.mly}.mli
OCAMLYACC_FILE_ML=${OCAMLYACC_FILE%.mly}.ml
OCAMLLEX_FILE_MLI=${OCAMLLEX_FILE%.mll}.mli
OCAMLLEX_FILE_ML=${OCAMLLEX_FILE%.mll}.ml
OCAMLLEX_FILE_ML=${OCAMLLEX_FILE%.mll}.ml
#OCAML_PARSER=${GENERIC_OCAML_PARSER%.ml}
#OCAML_PARSER=${GENERIC_OCAML_PARSER_NAME%.ml}

ocamlc -c ${AST_DATATYPE_FILE} -I ${BUILDDIR} # syntax.ml
ocamlc -c ${AST2LIST_FILE} -I ${BUILDDIR} # ast_to_sexpr.ml
ocamllex ${OCAMLLEX_FILE} > /dev/null # lexer.mll
ocamlyacc ${OCAMLYACC_FILE} # parser.mly
ocamlc -c ${OCAMLYACC_FILE_MLI} -I ${BUILDDIR} # parser.mli
ocamlc -c ${OCAMLLEX_FILE_ML} -I ${BUILDDIR} # lexer.ml
ocamlc -c ${OCAMLYACC_FILE_ML} -I ${BUILDDIR} # parser.ml
ocamlc -c ${GENERIC_OCAML_PARSER} -I ${BUILDDIR} # main.ml
ocamlc -o \
  ${BUILDDIR}/${GENERIC_OCAML_PARSER_NAME} \
  ${OCAMLLEX_FILE%.mll}.cmo \
  ${OCAMLYACC_FILE%.mly}.cmo \
  ${AST2LIST_FILE%.ml}.cmo \
  ${GENERIC_OCAML_PARSER%.ml}.cmo \
  ${AST_DATATYPE_FILE%.ml}.cmo
#mv -f ${OCAML_PARSER} ${BUILDDIR}
if [ $? -ne 0 ]; then
  echo "STOP"
  exit 1
fi

##################
# Output a specific script that works as an interpreter

echo "Generating `basename ${INTERP}`..."
mv -f ${INTERP} ${INTERP}.orig > /dev/null 2>&1
echo "#!/bin/bash" >> ${INTERP}
echo "INPUT_FILE=\$1" >> ${INTERP}
echo ${BUILDDIR}/${GENERIC_OCAML_PARSER_NAME} \< '${INPUT_FILE}' \> \${INPUT_FILE%.txt}.sexp  >> ${INTERP}
echo racket ${GENERIC_RUNNER} ${LANG_FILE} '${INPUT_FILE%.txt}.sexp' >> ${INTERP}
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
  echo ${BUILDDIR}/${GENERIC_OCAML_PARSER_NAME} \< '${INPUT_FILE}' \> \${INPUT_FILE%.txt}.sexp  >> ${PARSER}
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


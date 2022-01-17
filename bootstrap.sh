LLVM_HOME=$(llvm-config --prefix)
OPERATION_KIND_FILE=$LLVM_HOME/include/clang/AST/OperationKinds.def

gen_binary_operator_kind() {
  TARGET=src/binaryOperatorKind.ml
  echo "type t =" >$TARGET
  for line in $(grep "^BINARY_OPERATION" $OPERATION_KIND_FILE | cut -f 2 -d '(' | cut -f 1 -d ','); do
    echo "  | $line" >>$TARGET
  done
  echo "[@@deriving show]" >>$TARGET
}

gen_unary_operator_kind() {
  TARGET=src/unaryOperatorKind.ml
  echo "type t =" >$TARGET
  for line in $(grep "^UNARY_OPERATION" $OPERATION_KIND_FILE | cut -f 2 -d '(' | cut -f 1 -d ','); do
    echo "  | $line" >>$TARGET
  done
  echo "[@@deriving show]" >>$TARGET
}

gen_binary_operator_kind
gen_unary_operator_kind

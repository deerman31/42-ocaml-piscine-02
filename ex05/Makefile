# OCaml native compiler and tools
OCAMLOPT = ocamlopt
OCAMLDEP = ocamldep
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc

# Compiler flags
OCAMLFLAGS = -O3                # 最適化レベル最大
OCAMLFLAGS += -strict-sequence  # 式の順序を厳密にチェック
OCAMLFLAGS += -strict-formats   # format文字列を厳密にチェック
OCAMLFLAGS += -safe-string      # 文字列を不変として扱う
OCAMLFLAGS += -warn-error +A    # すべての警告をエラーとして扱う
OCAMLFLAGS += -w +A             # すべての警告を有効化
OCAMLFLAGS += -w -4-32-70      # 一部の冗長な警告を除外
OCAMLFLAGS += -g               # デバッグ情報を含める
OCAMLFLAGS += -opaque          # モジュール間の透過的な最適化を無効化
OCAMLFLAGS += -unboxed-types   # プリミティブ型の最適化

# Directories
SRC_DIR = src
BUILD_DIR = build

# Source files
ML_FILES = $(wildcard $(SRC_DIR)/*.ml)
MLI_FILES = $(wildcard $(SRC_DIR)/*.mli)

# Object and interface files
CMX_FILES = $(patsubst $(SRC_DIR)/%.ml,$(BUILD_DIR)/%.cmx,$(ML_FILES))
OBJ_FILES = $(patsubst $(SRC_DIR)/%.ml,$(BUILD_DIR)/%.o,$(ML_FILES))

# Main executable
MAIN = ft_test_sign
TARGET = $(BUILD_DIR)/$(MAIN)

# Default target
all: native

# Create build directory
$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# Native compilation
native: $(BUILD_DIR) $(TARGET)

# Rule for .ml to .cmx compilation
$(BUILD_DIR)/%.cmx: $(SRC_DIR)/%.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -c -o $@ $<

# Link the executable
$(TARGET): $(CMX_FILES)
	$(OCAMLOPT) $(OCAMLFLAGS) -o $@ $^

# Clean build artifacts
clean:
	rm -rf $(BUILD_DIR)

re:
	$(MAKE) clean
	$(MAKE) all

run:
	$(MAKE) re
	./$(TARGET)

# Generate dependencies
depend:
	$(OCAMLDEP) $(ML_FILES) $(MLI_FILES) > .depend

-include .depend

.PHONY: all native clean re run depend
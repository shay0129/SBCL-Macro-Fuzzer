# SBCL 2.3.2 Source Code Analysis Guide

## Overview
This guide provides a systematic approach to analyzing SBCL 2.3.2 source code for vulnerability research, specifically focusing on macro processing and compilation pipeline. This version matches the x86 binary for easier IDA analysis.

## Key Directories for Macro Research

### 1. Core Macro System
- **src/compiler/**: The main compiler system
  - `macros.lisp` - Core macro definitions
  - `ir1-translators.lisp` - IR1 translation for special forms
  - `ir1.lisp` - First intermediate representation
  - `ir1opt.lisp` - IR1 optimization passes

### 2. Reader and Parser
- **src/code/reader.lisp**: The Lisp reader that parses source code
  - Potential for parser confusion attacks
  - Symbol table manipulation
  - Read-time evaluation vulnerabilities

### 3. Evaluation Engine
- **src/code/eval.lisp**: Core evaluation logic
  - Macro expansion engine
  - Environment manipulation
  - Symbol resolution

### 4. Cross-Compilation
- **src/compiler/xc-main.lisp**: Cross-compilation entry point
  - Build-time macro processing
  - Potential for supply chain attacks

## Vulnerability Research Areas

### 1. Macro Expansion Bombs
**Location**: `src/compiler/macros.lisp`
**Attack Vector**: Exponential expansion causing resource exhaustion

### 2. Symbol Table Corruption
**Location**: `src/code/symbol.lisp`
**Attack Vector**: Malformed symbols causing memory corruption

### 3. Read-Time Evaluation
**Location**: `src/code/reader.lisp`
**Attack Vector**: #. reader macro exploitation

### 4. Cross-Compilation Attacks
**Location**: `src/compiler/xc-main.lisp`
**Attack Vector**: Malicious code injection during build

## Research Methodology

### Phase 1: Static Analysis
```bash
# Search for macro-related functions
grep -r "defmacro\|macro-function\|macroexpand" sbcl-source/src/
grep -r "reader-macro\|read-time" sbcl-source/src/
grep -r "eval-when\|compile-time" sbcl-source/src/
```

### Phase 2: Dynamic Analysis
```bash
# Find all macro expansion points
grep -r "expand\|transform" sbcl-source/src/compiler/
# Look for recursive processing
grep -r "recursive\|loop" sbcl-source/src/compiler/
```

### Phase 3: Vulnerability Patterns
```bash
# Buffer overflow patterns
grep -r "buffer\|array-bounds" sbcl-source/src/
# Memory allocation patterns
grep -r "allocate\|malloc\|memory" sbcl-source/src/
# Recursive processing without limits
grep -r "depth\|limit\|bound" sbcl-source/src/
```

## Interesting Files to Analyze

### High Priority
1. `src/compiler/macros.lisp` - Core macro system
2. `src/code/reader.lisp` - Input parsing
3. `src/code/eval.lisp` - Evaluation engine
4. `src/compiler/ir1-translators.lisp` - Special form handling

### Medium Priority
1. `src/code/symbol.lisp` - Symbol table management
2. `src/code/package.lisp` - Package system
3. `src/compiler/ir1opt.lisp` - Optimization passes
4. `src/compiler/constraint.lisp` - Type constraints

### Low Priority
1. `src/code/condition.lisp` - Error handling
2. `src/code/defstruct.lisp` - Structure definitions
3. `src/compiler/dump.lisp` - Compilation output

## Common Vulnerability Patterns

### 1. Unbounded Recursion
```lisp
(defmacro dangerous-macro (x)
  (if (zerop x)
      0
      `(+ 1 (dangerous-macro ,(1- x)))))
```

### 2. Symbol Interning Attacks
```lisp
(defmacro intern-bomb ()
  (dotimes (i 1000000)
    (intern (format nil "SYMBOL-~D" i))))
```

### 3. Read-Time Evaluation
```lisp
#.(progn
   (delete-file "important-file.txt")
   'harmless-value)
```

### 4. Macro Expansion Bombs
```lisp
(defmacro bomb (n)
  (if (zerop n)
      'done
      `(progn (bomb ,(1- n)) (bomb ,(1- n)))))
```

## Research Tools

### 1. Source Code Grep Commands
```bash
# Find all macro definitions
grep -r "defmacro" sbcl-source/src/ | head -20

# Find reader macros
grep -r "set-macro-character\|get-macro-character" sbcl-source/src/

# Find eval-when usage
grep -r "eval-when" sbcl-source/src/

# Find recursive functions
grep -r "labels\|flet.*recursive" sbcl-source/src/
```

### 2. Pattern Analysis
```bash
# Look for potential buffer overflows
grep -r "length\|size\|bounds" sbcl-source/src/code/

# Find memory allocation
grep -r "make-array\|allocate" sbcl-source/src/

# Look for error handling
grep -r "error\|signal\|warn" sbcl-source/src/code/
```

### 3. Cross-Reference Analysis
```bash
# Find all calls to macroexpand
grep -r "macroexpand" sbcl-source/src/ | wc -l

# Find all uses of eval
grep -r "\beval\b" sbcl-source/src/ | grep -v "eval-when"
```

## Next Steps

1. **Start with High Priority Files**: Focus on `macros.lisp` and `reader.lisp`
2. **Create Test Cases**: Develop specific test cases for each vulnerability pattern
3. **Run Fuzzer**: Use the fuzzer to generate test cases based on source analysis
4. **Monitor Crashes**: Look for segfaults, infinite loops, or resource exhaustion
5. **Analyze Results**: Correlate crashes with specific source code patterns

## Notes

- SBCL is written in Common Lisp, so understanding Lisp semantics is crucial
- Many vulnerabilities may be logic bugs rather than memory corruption
- Cross-compilation phase is particularly interesting for supply chain attacks
- Reader macros are a high-risk area due to their power and complexity

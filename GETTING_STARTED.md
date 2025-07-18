# SBCL Setup and Research Plan

## Getting Started Checklist

### Phase 1: Environment Setup
- [x] Download SBCL 2.3.2 for Windows x86 (32-bit): https://prdownloads.sourceforge.net/sbcl/sbcl-2.3.2-x86-windows-binary.msi
- [x] Install SBCL in accessible directory (default: C:\Program Files (x86)\Steel Bank Common Lisp\)
- [x] Test basic SBCL functionality
- [x] Verify Python fuzzer works
- [x] Test harness functionality

### Phase 2: Initial Fuzzing
- [x] Run 100 basic test cases
- [x] Check for any immediate crashes
- [x] Verify fuzzer generates valid LISP code
- [x] Test different macro types
- [x] Monitor system resources

**Initial Results:**
- 🎯 **60 test cases executed**
- 🐛 **12 timeout cases found (20% of tests)**
- 💡 **Timeouts indicate potential infinite loops or resource exhaustion**
- 🔍 **Found parser issues with malformed LISP syntax**
- 📊 **76% success rate, 24% interesting cases**

### Phase 3: Analysis Setup
- [ ] Set up crash dump collection
- [ ] Configure GDB for debugging
- [ ] Test crash analysis scripts
- [ ] Set up result logging
- [ ] Create crash triage workflow

### Phase 4: Systematic Testing
- [ ] Run 1000+ test cases
- [ ] Collect and analyze results
- [ ] Identify interesting crash patterns
- [ ] Refine fuzzing strategies
- [ ] Document findings

## Commands to Run

### SBCL Installation Steps:
```powershell
# 1. Download the x86 (32-bit) MSI installer for better IDA compatibility
# Go to: https://prdownloads.sourceforge.net/sbcl/sbcl-2.3.2-x86-windows-binary.msi
# Or run this PowerShell command:
Invoke-WebRequest -Uri "https://prdownloads.sourceforge.net/sbcl/sbcl-2.3.2-x86-windows-binary.msi" -OutFile "sbcl-x86-installer.msi"

# 2. Install SBCL
Start-Process "sbcl-x86-installer.msi" -Wait

# 3. Add SBCL to PATH (if not added automatically)
# SBCL x86 usually installs to: C:\Program Files (x86)\Steel Bank Common Lisp\
# Add this to your PATH environment variable
```

### Basic SBCL Testing:
```bash
# Test SBCL installation
sbcl --version

# Test basic LISP execution
sbcl --eval "(print 'hello)" --quit

# Test macro functionality
sbcl --eval "(defmacro test () '(print 'works)) (test)" --quit
```

### Fuzzer Testing:
```bash
# Basic fuzzer run
python fuzzer/fuzzer.py --num-tests 50

# With specific SBCL path
python fuzzer/fuzzer.py --sbcl-path "C:\path\to\sbcl.exe" --num-tests 100

# Using test harness
python fuzzer/harness.py --test-dir fuzzer/corpus --timeout 10
```

### Advanced Testing:
```bash
# Test with debugging
gdb --batch --ex "run --load test.lisp --quit" --ex "bt" sbcl

# Monitor with system tools
# Use Process Monitor or similar to watch file/memory access
```

## Research Notes

### Why x86 (32-bit) Version?
1. **Better IDA Support**: 32-bit binaries are easier to analyze in IDA
2. **Simpler Address Space**: Easier to understand memory layout
3. **More Debugging Tools**: Better tooling support for 32-bit analysis
4. **Crash Analysis**: Simpler stack traces and memory dumps

### Why Not Start with IDA?
1. SBCL is open source - source code analysis is more effective
2. We're testing behavior, not reverse engineering
3. Fuzzing finds runtime issues, not static code problems
4. Need working fuzzer before analyzing crashes
5. **But**: IDA will be useful for crash analysis after finding vulnerabilities

### Analysis Workflow:
1. **Fuzzing First**: Find crashes with automated fuzzing
2. **Triage**: Categorize crashes and identify interesting ones
3. **IDA Analysis**: Use IDA to understand crash root causes
4. **Exploit Development**: Create reliable exploits

### Key Focus Areas:
1. Macro expansion edge cases
2. Memory management during macro processing
3. Recursive macro definitions
4. Invalid macro syntax handling
5. Resource exhaustion scenarios

### Expected Findings:
- Stack overflow from deep macro recursion
- Memory leaks in macro expansion
- Parser errors with malformed input
- Potential buffer overflows in string handling

## Resources

### SBCL Documentation:
- https://www.sbcl.org/manual/
- https://github.com/sbcl/sbcl (source code)

### Fuzzing References:
- AFL++ documentation
- Fuzzing techniques for interpreters
- LISP-specific attack vectors

### Analysis Tools:
- **IDA Pro**: For 32-bit binary analysis and crash investigation
- **x32dbg**: Free alternative to IDA for dynamic analysis
- **Process Monitor**: System call monitoring
- **Application Verifier**: Windows heap debugging
- **GDB**: If using WSL or Linux analysis
- **Valgrind**: Memory debugging (Linux/WSL only)

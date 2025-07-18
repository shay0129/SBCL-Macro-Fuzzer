"""
IDA Pro Python Script for SBCL Macro Analysis
This script helps analyze SBCL's macro variable isolation mechanism
"""

import idc
import idaapi
import idautils

def find_macro_functions():
    """Find functions related to macro processing"""
    macro_keywords = [
        "macro", "expand", "defmacro", "let", "bind", 
        "scope", "symbol", "lexenv", "eval"
    ]
    
    found_functions = []
    
    # Search in function names
    for func_ea in idautils.Functions():
        func_name = idc.get_func_name(func_ea)
        if func_name:
            for keyword in macro_keywords:
                if keyword.lower() in func_name.lower():
                    found_functions.append((func_ea, func_name, "function_name"))
                    print(f"Found function: {func_name} at {hex(func_ea)}")
    
    # Search in strings
    for string_ea in idautils.Strings():
        string_val = str(string_ea)
        for keyword in macro_keywords:
            if keyword.lower() in string_val.lower():
                # Find references to this string
                for ref in idautils.DataRefsTo(string_ea.ea):
                    func_ea = idc.get_func_attr(ref, idc.FUNCATTR_START)
                    if func_ea != idc.BADADDR:
                        func_name = idc.get_func_name(func_ea)
                        found_functions.append((func_ea, func_name, f"string_ref: {string_val[:50]}"))
                        print(f"Found function via string: {func_name} at {hex(func_ea)} (refs: '{string_val[:50]}')")
    
    return found_functions

def analyze_stack_frames():
    """Analyze stack frame setup in macro-related functions"""
    macro_functions = find_macro_functions()
    
    for func_ea, func_name, discovery_method in macro_functions:
        print(f"\n--- Analyzing {func_name} ({discovery_method}) ---")
        
        # Get function boundaries
        func_end = idc.get_func_attr(func_ea, idc.FUNCATTR_END)
        
        # Analyze instructions in function
        ea = func_ea
        stack_operations = []
        
        while ea < func_end:
            disasm = idc.generate_disasm_line(ea, 0)
            mnem = idc.print_insn_mnem(ea)
            
            # Look for stack operations
            if mnem in ['push', 'pop', 'sub', 'add', 'mov', 'lea']:
                if 'esp' in disasm.lower() or 'ebp' in disasm.lower():
                    stack_operations.append((ea, disasm))
            
            ea = idc.next_head(ea)
        
        # Print interesting stack operations
        if stack_operations:
            print(f"Stack operations in {func_name}:")
            for op_ea, op_disasm in stack_operations[:10]:  # Limit output
                print(f"  {hex(op_ea)}: {op_disasm}")

def find_symbol_table_access():
    """Find code that accesses symbol tables"""
    # Look for common symbol table operations
    symbol_patterns = [
        "symbol", "intern", "lookup", "binding", "table", "hash"
    ]
    
    found_accesses = []
    
    for string_ea in idautils.Strings():
        string_val = str(string_ea)
        for pattern in symbol_patterns:
            if pattern.lower() in string_val.lower():
                # Find code references
                for ref in idautils.DataRefsTo(string_ea.ea):
                    func_ea = idc.get_func_attr(ref, idc.FUNCATTR_START)
                    if func_ea != idc.BADADDR:
                        func_name = idc.get_func_name(func_ea)
                        found_accesses.append((func_ea, func_name, string_val))
    
    return found_accesses

def create_analysis_report():
    """Generate a comprehensive analysis report"""
    print("=" * 60)
    print("SBCL MACRO ANALYSIS REPORT")
    print("=" * 60)
    
    print("\n1. MACRO-RELATED FUNCTIONS:")
    macro_funcs = find_macro_functions()
    for func_ea, func_name, method in macro_funcs:
        print(f"  {func_name} @ {hex(func_ea)} (found via: {method})")
    
    print(f"\nTotal macro-related functions found: {len(macro_funcs)}")
    
    print("\n2. SYMBOL TABLE ACCESS POINTS:")
    symbol_accesses = find_symbol_table_access()
    for func_ea, func_name, string_ref in symbol_accesses:
        print(f"  {func_name} @ {hex(func_ea)} (refs: {string_ref[:30]})")
    
    print("\n3. STACK FRAME ANALYSIS:")
    analyze_stack_frames()
    
    print("\n4. BREAKPOINT SUGGESTIONS:")
    print("Set breakpoints on these functions for dynamic analysis:")
    for func_ea, func_name, _ in macro_funcs[:5]:  # Top 5 functions
        print(f"  bp {hex(func_ea)}  # {func_name}")

def setup_dynamic_analysis():
    """Setup for dynamic analysis with breakpoints"""
    macro_funcs = find_macro_functions()
    
    if not macro_funcs:
        print("No macro functions found. Run static analysis first.")
        return
    
    print("Setting up dynamic analysis...")
    
    # Set breakpoints on key functions
    for func_ea, func_name, _ in macro_funcs[:3]:  # Top 3 functions
        idc.add_bpt(func_ea)
        print(f"Breakpoint set on {func_name} @ {hex(func_ea)}")
    
    print("\nBreakpoints set. Now run your test case and analyze the execution flow.")

# Main execution
if __name__ == "__main__":
    print("Starting SBCL Macro Analysis...")
    create_analysis_report()
    
    # Uncomment the next line when ready for dynamic analysis
    # setup_dynamic_analysis()

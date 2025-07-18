"""
GDB Analysis Script for SBCL Macro Variable Isolation
Use this script to trace macro execution and variable binding
"""

import gdb
import re

class SBCLMacroTracer:
    def __init__(self):
        self.macro_breakpoints = []
        self.variable_watches = []
        self.execution_log = []
    
    def setup_macro_tracing(self):
        """Setup tracing for macro expansion"""
        # Common SBCL internal function patterns
        macro_functions = [
            "*expand-macro*",
            "*eval*", 
            "*defmacro*",
            "*let*",
            "*symbol-binding*"
        ]
        
        print("Setting up macro tracing...")
        
        # Try to set breakpoints on common patterns
        for pattern in macro_functions:
            try:
                gdb.execute(f"break {pattern}")
                print(f"Breakpoint set on {pattern}")
            except:
                pass
        
        # Set breakpoint on main entry point
        try:
            gdb.execute("break main")
            print("Breakpoint set on main")
        except:
            pass
    
    def trace_variable_binding(self, var_name):
        """Trace the binding of a specific variable"""
        print(f"Tracing variable: {var_name}")
        
        # Set watchpoint on variable if possible
        try:
            gdb.execute(f"watch {var_name}")
            self.variable_watches.append(var_name)
        except:
            print(f"Could not set watchpoint on {var_name}")
    
    def analyze_stack_frame(self):
        """Analyze current stack frame for variable bindings"""
        try:
            frame_info = gdb.execute("info frame", to_string=True)
            locals_info = gdb.execute("info locals", to_string=True)
            args_info = gdb.execute("info args", to_string=True)
            
            print("=== STACK FRAME ANALYSIS ===")
            print("Frame info:")
            print(frame_info)
            print("\nLocal variables:")
            print(locals_info)
            print("\nArguments:")
            print(args_info)
            print("=" * 30)
            
        except Exception as e:
            print(f"Error analyzing stack frame: {e}")
    
    def dump_memory_region(self, address, size=256):
        """Dump memory region for analysis"""
        try:
            result = gdb.execute(f"x/{size}x {address}", to_string=True)
            print(f"Memory dump at {address}:")
            print(result)
        except Exception as e:
            print(f"Error dumping memory: {e}")
    
    def log_execution_point(self):
        """Log current execution point"""
        try:
            pc = gdb.parse_and_eval("$pc")
            func_info = gdb.execute("info symbol $pc", to_string=True)
            
            log_entry = {
                'pc': str(pc),
                'function': func_info.strip(),
                'timestamp': gdb.execute("info proc", to_string=True)
            }
            
            self.execution_log.append(log_entry)
            print(f"Logged execution at {pc}: {func_info.strip()}")
            
        except Exception as e:
            print(f"Error logging execution: {e}")

# GDB Commands for SBCL Analysis
class SBCLAnalysisCommands:
    
    def __init__(self):
        self.tracer = SBCLMacroTracer()
    
    def setup_sbcl_debugging(self):
        """Initial setup for SBCL debugging"""
        commands = [
            "set confirm off",
            "set pagination off", 
            "set print pretty on",
            "set print symbol-filename on",
            "handle SIGPIPE nostop noprint",
            "handle SIGUSR1 nostop noprint",
            "handle SIGUSR2 nostop noprint"
        ]
        
        for cmd in commands:
            try:
                gdb.execute(cmd)
                print(f"Executed: {cmd}")
            except Exception as e:
                print(f"Failed to execute {cmd}: {e}")
    
    def run_collision_test(self):
        """Run our collision test under GDB"""
        print("Running collision test under debugger...")
        
        # Setup tracing
        self.tracer.setup_macro_tracing()
        
        # Run the program with our test
        try:
            gdb.execute("run --script collision_attack_test.lisp")
        except:
            print("Program execution stopped (expected with breakpoints)")
    
    def continue_with_analysis(self):
        """Continue execution while analyzing each breakpoint"""
        while True:
            try:
                print("\n--- Continuing execution ---")
                self.tracer.analyze_stack_frame()
                self.tracer.log_execution_point()
                
                # Continue to next breakpoint
                gdb.execute("continue")
                
            except gdb.error as e:
                if "not being run" in str(e):
                    print("Program finished execution")
                    break
                else:
                    print(f"GDB error: {e}")
                    break
            except KeyboardInterrupt:
                print("Analysis interrupted by user")
                break

# Usage instructions as comments:
"""
GDB Usage Instructions:

1. Start GDB with SBCL:
   gdb --args sbcl --script collision_attack_test.lisp

2. In GDB, run Python script:
   (gdb) python exec(open('sbcl_gdb_analysis.py').read())

3. Setup debugging:
   (gdb) python analyzer = SBCLAnalysisCommands()
   (gdb) python analyzer.setup_sbcl_debugging()

4. Run analysis:
   (gdb) python analyzer.run_collision_test()
   (gdb) python analyzer.continue_with_analysis()

5. Manual analysis commands:
   (gdb) info registers
   (gdb) x/20x $esp
   (gdb) bt
   (gdb) info locals
"""

print("SBCL GDB Analysis Script loaded")
print("Use: analyzer = SBCLAnalysisCommands() to start")

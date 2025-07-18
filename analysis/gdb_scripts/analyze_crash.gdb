# GDB script for analyzing SBCL crashes
# Save as analyze_crash.gdb

# Set up environment
set confirm off
set verbose off
set height 0
set width 0

# Define useful functions
define crash_info
    echo \n=== CRASH ANALYSIS ===\n
    info registers
    echo \n=== STACK TRACE ===\n
    bt
    echo \n=== STACK FRAME INFO ===\n
    info frame
    echo \n=== LOCAL VARIABLES ===\n
    info locals
    echo \n=== MEMORY AROUND CRASH ===\n
    x/20i $pc-40
    echo \n=== END ANALYSIS ===\n
end

# Run program and analyze crash
run
crash_info

# Save output to file
set logging file crash_analysis.txt
set logging on
crash_info
set logging off

quit

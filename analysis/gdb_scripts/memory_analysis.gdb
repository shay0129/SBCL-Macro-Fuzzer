# GDB script for memory analysis
# Save as memory_analysis.gdb

# Set up environment
set confirm off
set verbose off

# Define memory analysis functions
define heap_info
    echo \n=== HEAP ANALYSIS ===\n
    info proc mappings
    echo \n=== MEMORY REGIONS ===\n
    maintenance info sections
end

define stack_analysis
    echo \n=== STACK ANALYSIS ===\n
    info stack
    echo \n=== STACK MEMORY ===\n
    x/100x $sp-200
end

define memory_corruption_check
    echo \n=== MEMORY CORRUPTION CHECK ===\n
    # Check for common corruption patterns
    x/20x $pc-20
    x/20x $sp-20
    info registers
end

# Main analysis
run
heap_info
stack_analysis
memory_corruption_check

quit

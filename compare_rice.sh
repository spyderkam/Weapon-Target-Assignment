
#!/usr/bin/env bash

# Variables to modify
v=10.0
b=75.0

echo "========================================"
echo "Comparing Rice Distribution Results"
echo "Parameters: v=${v}, b=${b}"
echo "========================================"
echo ""

# Compile Ada version (if not already compiled or if source changed)
echo "--- Ada Result ---"
cd src/rice/ada
gnatmake test_rice_simple.adb -q 2>/dev/null
if [ $? -eq 0 ]; then
    ./test_rice_simple ${v} ${b}
else
    echo "Ada compilation failed"
fi
cd - > /dev/null

echo ""

# Run Python version
echo "--- Python Result ---"
python3 -c "
from src.rice.python.genRiceTable import calc_rice_cdf_asymp

v = ${v}
b = ${b}
sig = 1.0

p, q = calc_rice_cdf_asymp(v, sig, b, calc_q=False)

print(f'Asymptotic (p) at (v={v}, b={b}) = {p:.6f}')
"

echo ""
echo "========================================"

# Cleanup compiled Ada files
cd src/rice/ada
rm -f test_rice_simple
rm -f *.ali *.o
cd - > /dev/null

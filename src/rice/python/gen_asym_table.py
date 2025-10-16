from genRiceTable import gen_rice_table
import numpy as np

# Generate the 150x150 asymptotic table
T_asymptotic = gen_rice_table()

# Save as CSV
np.savetxt("riceTable.csv", T_asymptotic, delimiter=',', fmt='%.4f')
print("\nSaved 150×150 asymptotic table to: riceTable.csv")

# Save as CSV with b & v values
v_vals = np.linspace(2, 150, 150)
b_vals = np.linspace(48.6846, 150, 150)

# Create augmented matrix with v and b values
# First row: empty cell followed by b values
# First column: v values
augmented = np.zeros((151, 151))
augmented[0, 1:] = b_vals  # First row (b values)
augmented[1:, 0] = v_vals  # First column (v values)
augmented[1:, 1:] = T_asymptotic  # The 150x150 data matrix

np.savetxt("riceTable_bv.csv", augmented, delimiter=',', fmt='%.4f')
print("Saved 150×150 asymptotic table with b and v values to: riceTable_bv.csv")
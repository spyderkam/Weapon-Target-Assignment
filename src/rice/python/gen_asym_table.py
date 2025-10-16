from genRiceTable import gen_rice_table
import numpy as np

# Generate the 150x150 asymptotic table
T_asymptotic = gen_rice_table()

# Save as CSV
np.savetxt("riceTable.csv", T_asymptotic, delimiter=',', fmt='%.4f')
print("\nSaved 150×150 asymptotic table to: riceTable.csv")

# Save as CSV with b & v values
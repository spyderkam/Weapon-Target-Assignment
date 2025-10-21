#!/usr/bin/env python3

import csv

n = 5  # Number of values per line

def read_matrix_from_csv(csv_file_path):
    with open(csv_file_path, mode='r', newline='') as csv_file:
        reader = csv.reader(csv_file)
        matrix = [list(map(str.strip, row)) for row in reader]
    return matrix

def format_row(row, row_number):
    # Check if row number would be 4 digits (>= 1000)
    num_rows = row_number
    if num_rows >= 1000:
        raise ValueError(f"Error: Row number {num_rows} exceeds 3-digit limit (max 999 rows allowed)")
    
    # Calculate indentation based on row number digits
    num_digits = len(str(row_number))
    if num_digits == 1:
        indent = "      "    # 6 spaces to align with "1 => ("
    elif num_digits == 2:
        indent = "       "   # 7 spaces to align with "10 => ("
    else:  # 3 digits
        indent = "        "  # 8 spaces to align with "100 => ("
    
    # Group values into sets of n
    lines = []
    for i in range(0, len(row), n):
        group = row[i:i+n]
        # Format with comma after each value
        formatted_group = ", ".join(group)
        lines.append(formatted_group)
    
    # Build the output
    if not lines: return f"{row_number} => ()"
    # First line starts with row number
    result = f"{row_number} => ({lines[0]}"
    # Add subsequent lines with proper indentation
    for line in lines[1:]: result += ",\n" + indent + line
    # Close the parenthesis
    result += "),"
    
    return result

def write_matrix_to_txt(matrix, txt_file_path):
    # Check total number of rows
    if len(matrix) >= 1000:
        raise ValueError(f"Error: CSV has {len(matrix)} rows, exceeding 3-digit limit (max 999 rows allowed)")
    
    with open(txt_file_path, mode='w') as txt_file:
        total_rows = len(matrix)
        for row_number, row in enumerate(matrix, start=1):
            formatted_row = format_row(row, row_number)
            # Remove trailing comma if this is the last row
            if row_number == total_rows and formatted_row.endswith('),'):
                formatted_row = formatted_row[:-1]  # Remove the last character (comma)
            txt_file.write(formatted_row + '\n')

if __name__ == "__main__":
    csv_file_path = 'riceTable.csv'            # Input CSV file path
    txt_file_path = 'riceTable_formatted.txt'  # Output TXT file path
    
    # Read the matrix from CSV
    matrix = read_matrix_from_csv(csv_file_path)
    # Write the formatted output to TXT
    write_matrix_to_txt(matrix, txt_file_path)
    
    print(f"Matrix has been successfully written to {txt_file_path}")

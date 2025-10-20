#!/usr/bin/env python3

import csv

def read_matrix_from_csv(csv_file_path):
    with open(csv_file_path, mode='r', newline='') as csv_file:
        reader = csv.reader(csv_file)
        matrix = [list(map(str.strip, row)) for row in reader]
    return matrix

def format_row(row, row_number):
    groups = [f"{row[i]} {row[i+1]} {row[i+2]}" for i in range(0, len(row), 3)]
    # Join the groups with 5 spaces
    formatted_row = '     '.join(groups)
    return f"{row_number} => ({formatted_row})"

def write_matrix_to_txt(matrix, txt_file_path):
    with open(txt_file_path, mode='w') as txt_file:
        for row_number, row in enumerate(matrix, start=1):
            formatted_row = format_row(row, row_number)
            txt_file.write(formatted_row + '\n')

def main():
    csv_file_path = 'riceTable.csv'            # Input CSV file path
    txt_file_path = 'riceTable_formatted.txt'  # Output TXT file path
    
    # Read the matrix from CSV
    matrix = read_matrix_from_csv(csv_file_path)
    
    # Write the formatted output to TXT
    write_matrix_to_txt(matrix, txt_file_path)
    
    print(f"Matrix has been successfully written to {txt_file_path}")

if __name__ == "__main__":
    main()
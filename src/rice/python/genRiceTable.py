#!/usr/bin/env python3

"""
Rice Distribution CDF Calculation and Lookup Table Generation
Python translation of MATLAB scripts for Rice distribution analysis
"""

import numpy as np
import pandas as pd
from pathlib import Path
from scipy import integrate, special
from scipy.stats import norm
from typing import Tuple, Optional


def calc_rice_cdf_asymp(v: float, sig: float, b: float) -> Tuple[float, float]:
    """
    Evaluate CDF of Rice distribution with offset v at x = b.
    
    Parameters
    ----------
    v : float
        Non-centrality parameter (offset from origin)
    sig : float
        Scale parameter (sigma)
    b : float
        Evaluation point (fuel budget)
    
    Returns
    -------
    p : float
        Asymptotic approximation of Rice CDF
    q : float
        Exact Rice CDF value
    """
    # Rescale inputs wrt sigma
    b_norm = b / sig
    v_norm = v / sig
    z = b_norm * v_norm
    
    # Compute PDF and CDF of Gaussian
    norm_coeff = 1 / np.sqrt(2 * np.pi)
    pdf_norm = norm_coeff * np.exp(-0.5 * (b_norm - v_norm)**2)
    
    # Using scipy's norm.cdf for the Gaussian CDF
    cdf_norm = norm.cdf(b_norm, loc=v_norm, scale=1)
    
    # Asymptotic calculation
    if z > 0:  # Avoid division by zero
        p = cdf_norm - pdf_norm / (2 * np.sqrt(z)) * (1 + 1 / (8 * z))
    else:
        p = cdf_norm
    
    # Exact calculation using numerical integration
    def integrand(x):
        return x * np.exp(-(x**2 + v_norm**2) / 2) * special.i0(v_norm * x)
    
    q, _ = integrate.quad(integrand, 0, b_norm)
    
    return p, q


def rice_cdf_table_small() -> np.ndarray:
    """
    Create lookup table for smaller offsets and fuel budgets (relative to sigma).
    Similar to rice_cdf_table.m
    
    Returns
    -------
    T : np.ndarray
        200x200 matrix of exact Rice CDF values
    """
    v_vals = np.linspace(0, 4, 200)
    b_vals = np.linspace(0, 4, 200)
    T = np.zeros((200, 200))
    
    print("Generating small value table (200x200)...")
    for i in range(200):
        if i % 20 == 0:
            print(f"Progress: {i/200*100:.1f}%")
        for j in range(200):
            _, T[i, j] = calc_rice_cdf_asymp(v_vals[i], 1, b_vals[j])
    
    return T


def gen_rice_table() -> np.ndarray:
    """
    Generate lookup table using asymptotic approximation.
    Translation of genRiceTable.m
    
    Returns
    -------
    T : np.ndarray
        150x150 matrix of asymptotic Rice CDF values
    """
    # Create lookup table for larger offsets (relative to sigma)
    v_vals = np.linspace(2, 150, 150)
    b_vals = np.linspace(48.6846, 150, 150)
    T = np.zeros((150, 150))
    
    print("Generating asymptotic table (150x150)...")
    counter = 0
    
    for i in range(150):
        if i % 10 == 0:
            print(f"Progress: {i/150*100:.1f}%")
        
        for j in range(150):
            # Note: Using sigma=10 as in the MATLAB code
            p, _ = calc_rice_cdf_asymp(v_vals[i], 10, b_vals[j])
            T[i, j] = p
            counter += 1
            
            # Clear screen occasionally (like MATLAB's \x1b[2J)
            if counter % 15000 == 0:
                print("\033[2J\033[H")  # Clear screen and move cursor to top
    
    return T


def save_rice_tables(output_dir: Optional[Path] = None) -> None:
    """
    Generate and save both Rice distribution lookup tables.
    
    Parameters
    ----------
    output_dir : Path, optional
        Directory to save output files. Defaults to current directory.
    """
    if output_dir is None:
        output_dir = Path.cwd()
    else:
        output_dir = Path(output_dir)
    
    # Generate large value table (asymptotic)
    print("=" * 50)
    print("Generating Rice distribution lookup tables")
    print("=" * 50)
    
    T_large = gen_rice_table()
    
    # Save as CSV
    large_csv_path = output_dir / "riceTable.csv"
    np.savetxt(large_csv_path, T_large, delimiter=',', fmt='%.4f')
    print(f"\nSaved large value table to: {large_csv_path}")
    
    # Save as numpy binary for efficient loading
    large_npy_path = output_dir / "rice_lookup_table.npy"
    np.save(large_npy_path, T_large)
    print(f"Saved large value table (binary) to: {large_npy_path}")
    
    # Generate small value table (exact)
    print("\n" + "=" * 50)
    T_small = rice_cdf_table_small()
    
    # Save as CSV using pandas for better formatting
    small_csv_path = output_dir / "rice_lookup.csv"
    df_small = pd.DataFrame(T_small)
    df_small.to_csv(small_csv_path, index=False, float_format='%.4f')
    print(f"\nSaved small value table to: {small_csv_path}")
    
    # Save as numpy binary
    small_npy_path = output_dir / "rice_lookup.npy"
    np.save(small_npy_path, T_small)
    print(f"Saved small value table (binary) to: {small_npy_path}")
    
    print("\n" + "=" * 50)
    print("Table generation complete!")
    print("=" * 50)


class RiceLookupTable:
    """
    Class for loading and interpolating Rice distribution lookup tables.
    """
    
    def __init__(self, table_path: Path):
        """
        Load a Rice distribution lookup table.
        
        Parameters
        ----------
        table_path : Path
            Path to .npy or .csv file containing the lookup table
        """
        if table_path.suffix == '.npy':
            self.table = np.load(table_path)
        elif table_path.suffix == '.csv':
            self.table = np.loadtxt(table_path, delimiter=',')
        else:
            raise ValueError(f"Unsupported file type: {table_path.suffix}")
        
        self.shape = self.table.shape
        
        # Define parameter ranges based on table size
        if self.shape == (150, 150):
            self.v_range = (2, 150)
            self.b_range = (48.6846, 150)
            self.sigma = 10
        elif self.shape == (200, 200):
            self.v_range = (0, 4)
            self.b_range = (0, 4)
            self.sigma = 1
        else:
            raise ValueError(f"Unexpected table shape: {self.shape}")
    
    def interpolate(self, v: float, b: float) -> float:
        """
        Interpolate CDF value from lookup table.
        
        Parameters
        ----------
        v : float
            Non-centrality parameter
        b : float
            Evaluation point
        
        Returns
        -------
        float
            Interpolated Rice CDF value
        """
        # Map v and b to table indices
        v_idx = (v - self.v_range[0]) / (self.v_range[1] - self.v_range[0]) * (self.shape[0] - 1)
        b_idx = (b - self.b_range[0]) / (self.b_range[1] - self.b_range[0]) * (self.shape[1] - 1)
        
        # Bilinear interpolation
        from scipy.interpolate import interp2d
        
        v_indices = np.arange(self.shape[0])
        b_indices = np.arange(self.shape[1])
        
        interp_func = interp2d(b_indices, v_indices, self.table, kind='linear')
        
        return float(interp_func(b_idx, v_idx))


def main():
    """Main function to generate all Rice distribution tables."""
    save_rice_tables()


if __name__ == "__main__":
    main()
    
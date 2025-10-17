
# Bilinear Interpolation

## Overview

Bilinear interpolation is a method for interpolating values within a 2D grid (matrix) using linear interpolation in two directions. Given a matrix $M$ and coordinates $(r, c)$ that fall between discrete grid points, bilinear interpolation estimates the value at that position using the four nearest grid points.

## Mathematical Formulation

### Problem Setup

Consider a matrix $M$ with discrete indices:

- Rows indexed by integer values $r \in \{1, 2, 3, \ldots, R\}$
- Columns indexed by integer values $c \in \{1, 2, 3, \ldots, C\}$

We wish to estimate the value at non-integer coordinates $(r_{\mathrm{query}}, c_{\mathrm{query}})$ where:

$$
1 \leq r_{\mathrm{query}} \leq R \quad \text{and} \quad 1 \leq c_{\mathrm{query}} \leq C
$$

### Step 1: Identify Corner Indices

First, identify the four corner points that form a rectangular cell containing the query point:

$$
r_{\mathrm{low}} = \lfloor r_{\mathrm{query}} \rfloor
$$

$$
r_{\mathrm{high}} = r_{\mathrm{low}} + 1
$$

$$
c_{\mathrm{low}} = \lfloor c_{\mathrm{query}} \rfloor
$$

$$
c_{\mathrm{high}} = c_{\mathrm{low}} + 1
$$

where $\lfloor \cdot \rfloor$ denotes the floor function.

### Step 2: Extract Corner Values

Extract the four corner values from the matrix:

$$
Q_{11} = M(r_{\mathrm{low}}, c_{\mathrm{low}})
$$

$$
Q_{12} = M(r_{\mathrm{low}}, c_{\mathrm{high}})
$$

$$
Q_{21} = M(r_{\mathrm{high}}, c_{\mathrm{low}})
$$

$$
Q_{22} = M(r_{\mathrm{high}}, c_{\mathrm{high}})
$$

These represent the four corners of the interpolation cell:

```
r_low, c_low        r_low, c_high
    Q₁₁ ──────────────── Q₁₂
     │                    │
     │    (r_query,       │
     │     c_query)       │
     │                    │
    Q₂₁ ──────────────── Q₂₂
r_high, c_low      r_high, c_high
```

### Step 3: Calculate Fractional Parts

Compute the fractional distances from the lower bounds:

$$
r_{\mathrm{frac}} = r_{\mathrm{query}} - r_{\mathrm{low}}
$$

$$
c_{\mathrm{frac}} = c_{\mathrm{query}} - c_{\mathrm{low}}
$$

These values represent how far the query point is from the lower-left corner, normalized to $[0, 1]$.

### Step 4: Interpolate Along Column Direction

Perform linear interpolation along the column direction (horizontally) for both the bottom and top edges:

$$
R_1 = Q_{11} \cdot (1 - c_{\mathrm{frac}}) + Q_{12} \cdot c_{\mathrm{frac}}
$$

$$
R_2 = Q_{21} \cdot (1 - c_{\mathrm{frac}}) + Q_{22} \cdot c_{\mathrm{frac}}
$$

where:
- $R_1$ is the interpolated value along the bottom edge (at row $r_{\mathrm{low}}$)
- $R_2$ is the interpolated value along the top edge (at row $r_{\mathrm{high}}$)

### Step 5: Interpolate Along Row Direction

Finally, perform linear interpolation along the row direction (vertically) between $R_1$ and $R_2$:

$$
M_{\mathrm{interp}} = R_1 \cdot (1 - r_{\mathrm{frac}}) + R_2 \cdot r_{\mathrm{frac}}
$$

This yields the final interpolated value at $(r_{\mathrm{query}}, c_{\mathrm{query}})$.

## Compact Form

The entire bilinear interpolation can be expressed in a single equation:

$$
M_{\mathrm{interp}} = \sum_{i=0}^{1} \sum_{j=0}^{1} Q_{(1+i)(1+j)} \cdot w_i^{(r)} \cdot w_j^{(c)}
$$

where the weights are:

$$
w_0^{(r)} = 1 - r_{\mathrm{frac}}, \quad w_1^{(r)} = r_{\mathrm{frac}}
$$

$$
w_0^{(c)} = 1 - c_{\mathrm{frac}}, \quad w_1^{(c)} = c_{\mathrm{frac}}
$$

## Boundary Handling

When $r_{\mathrm{query}}$ or $c_{\mathrm{query}}$ approaches the matrix boundaries, indices must be clamped:

$$
r_{\mathrm{low}}^{\mathrm{safe}} = \max(1, \min(R, r_{\mathrm{low}}))
$$

$$
r_{\mathrm{high}}^{\mathrm{safe}} = \max(1, \min(R, r_{\mathrm{high}}))
$$

$$
c_{\mathrm{low}}^{\mathrm{safe}} = \max(1, \min(C, c_{\mathrm{low}}))
$$

$$
c_{\mathrm{high}}^{\mathrm{safe}} = \max(1, \min(C, c_{\mathrm{high}}))
$$

Use these clamped indices when extracting corner values from $M$.

## Properties

1. **Exactness at grid points**: When $(r_{\mathrm{query}}, c_{\mathrm{query}})$ coincides with a grid point, the interpolation returns the exact matrix value.

2. **Continuity**: The interpolated function is continuous across cell boundaries.

3. **Linear preservation**: If the actual function is linear in either direction, bilinear interpolation reproduces it exactly within each cell.

4. **Computational efficiency**: Requires only 4 matrix lookups and 3 linear interpolations.

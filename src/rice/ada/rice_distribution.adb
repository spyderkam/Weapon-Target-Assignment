package body Rice_Distribution is

   -- Hard-coded lookup table (150x150 asymptotic table)
   -- This contains the values from riceTable.csv
   -- v values: 2 to 150 (150 points)
   -- b values: 48.6846 to 150 (150 points)
   -- sigma = 10
   
   Rice_Lookup : constant Rice_Table := (
      -- Row 1 (v = 2)
      1 => (others => 0.0),  -- Replace with actual values from CSV
      -- Row 2 (v ≈ 3.01)
      2 => (others => 0.0),  -- Replace with actual values from CSV
      -- ... Continue for all 150 rows
      others => (others => 0.0)  -- Placeholder
   );


   function Bilinear_Interpolate(
      v_idx : Float; 
      b_idx : Float; 
      Table : Rice_Table
   ) return Float is
      
      -- Get integer indices
      v_low  : Integer := Integer(Float'Floor(v_idx));
      v_high : Integer := v_low + 1;
      b_low  : Integer := Integer(Float'Floor(b_idx));
      b_high : Integer := b_low + 1;
      
      -- Get fractional parts
      v_frac : Float := v_idx - Float(v_low);
      b_frac : Float := b_idx - Float(b_low);
      
      -- Boundary checking
      v_low_safe  : Integer;
      v_high_safe : Integer;
      b_low_safe  : Integer;
      b_high_safe : Integer;
      
      -- Values at corners
      Q11, Q12, Q21, Q22 : Float;
      R1, R2             : Float;
      Result             : Float;
      
   begin
      -- Clamp indices to valid range [1..150]
      v_low_safe  := Integer'Max(1, Integer'Min(150, v_low));
      v_high_safe := Integer'Max(1, Integer'Min(150, v_high));
      b_low_safe  := Integer'Max(1, Integer'Min(150, b_low));
      b_high_safe := Integer'Max(1, Integer'Min(150, b_high));
      
      -- Get values at the four corners
      Q11 := Table(v_low_safe, b_low_safe);
      Q12 := Table(v_low_safe, b_high_safe);
      Q21 := Table(v_high_safe, b_low_safe);
      Q22 := Table(v_high_safe, b_high_safe);
      
      -- Interpolate along b direction first
      R1 := Q11 * (1.0 - b_frac) + Q12 * b_frac;
      R2 := Q21 * (1.0 - b_frac) + Q22 * b_frac;
      
      -- Then interpolate along v direction
      Result := R1 * (1.0 - v_frac) + R2 * v_frac;
      
      return Result;
   end Bilinear_Interpolate;
   
   
   function Get_Rice_CDF(v : Float; b : Float) return Float is
      v_idx  : Float;
      b_idx  : Float;
      Result : Float;
   begin
      -- Check if values are within table bounds
      if v < v_min or v > v_max or b < b_min or b > b_max then
         -- Return boundary value or raise exception
         -- For now, clamp to boundary
         return 0.0;  -- Or handle out-of-bounds case appropriately
      end if;
      
      -- Map v and b to table indices (1-based, range 1..150)
      v_idx := ((v - v_min) / (v_max - v_min)) * 149.0 + 1.0;
      b_idx := ((b - b_min) / (b_max - b_min)) * 149.0 + 1.0;
      
      -- Check if we hit exact table values (avoid unnecessary interpolation)
      if Float'Remainder(v_idx, 1.0) = 0.0 and Float'Remainder(b_idx, 1.0) = 0.0 then
         -- Exact table lookup
         return Rice_Lookup(Integer(v_idx), Integer(b_idx));
      else
         -- Bilinear interpolation
         return Bilinear_Interpolate(v_idx, b_idx, Rice_Lookup);
      end if;
   end Get_Rice_CDF;

end Rice_Distribution;

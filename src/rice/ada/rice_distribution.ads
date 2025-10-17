package Rice_Distribution is

   -- Type for the Rice distribution lookup table
   type Rice_Table is array (1..150, 1..150) of Float;
   
   -- Hard-coded lookup table (150x150 asymptotic table)
   -- This should contain the values from riceTable.csv
   -- v values: 2 to 150 (150 points)
   -- b values: 48.6846 to 150 (150 points)
   -- sigma = 10

   Rice_Lookup : constant Rice_Table := (
      -- Row 1 (v=2)
      1 => (others => 0.0),  -- Replace with actual values from CSV
      -- Row 2 (v=~3.01)
      2 => (others => 0.0),  -- Replace with actual values from CSV
      -- ... Continue for all 150 rows
      others => (others => 0.0)  -- Placeholder
   );
   
   -- Parameter ranges for the table
   V_Min : constant Float := 2.0;
   V_Max : constant Float := 150.0;
   B_Min : constant Float := 48.6846;
   B_Max : constant Float := 150.0;
   Sigma : constant Float := 10.0;
   
   -- Main function: Get Rice CDF value with interpolation
   function Get_Rice_CDF(V : Float; B : Float) return Float;
   
   -- Helper function: Bilinear interpolation
   function Bilinear_Interpolate(
      V_Idx : Float; 
      B_Idx : Float; 
      Table : Rice_Table
   ) return Float;

end Rice_Distribution;


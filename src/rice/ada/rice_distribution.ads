package Rice_Distribution is

   -- Type for the Rice distribution lookup table
   type Rice_Table is array (1..150, 1..150) of Float;

   -- Parameter ranges for the table
   v_min : constant Float := 2.0;
   v_max : constant Float := 150.0;
   b_min : constant Float := 48.6846;
   b_max : constant Float := 150.0;
   sigma : constant Float := 10.0;

   -- Main function: Get Rice CDF value with interpolation
   function Get_Rice_CDF(v : Float; b : Float) return Float;

   -- Helper function: Bilinear interpolation
   function Bilinear_Interpolate(
      v_idx : Float;
      b_idx : Float;
      Table : Rice_Table
   ) return Float;

end Rice_Distribution;
package Rice_Distribution is

   -- Type for the Rice distribution lookup table
   type Rice_Table is array (1..150, 1..150) of Float;

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
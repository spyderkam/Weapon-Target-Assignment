package body Rice_Distribution is

   function Bilinear_Interpolate(
      V_Idx : Float; 
      B_Idx : Float; 
      Table : Rice_Table
   ) return Float is
      
      -- Get integer indices
      V_Low : Integer := Integer(Float'Floor(V_Idx));
      V_High : Integer := V_Low + 1;
      B_Low : Integer := Integer(Float'Floor(B_Idx));
      B_High : Integer := B_Low + 1;
      
      -- Get fractional parts
      V_Frac : Float := V_Idx - Float(V_Low);
      B_Frac : Float := B_Idx - Float(B_Low);
      
      -- Boundary checking
      V_Low_Safe : Integer;
      V_High_Safe : Integer;
      B_Low_Safe : Integer;
      B_High_Safe : Integer;
      
      -- Values at corners
      Q11, Q12, Q21, Q22 : Float;
      R1, R2 : Float;
      Result : Float;
      
   begin
      -- Clamp indices to valid range [1..150]
      V_Low_Safe := Integer'Max(1, Integer'Min(150, V_Low));
      V_High_Safe := Integer'Max(1, Integer'Min(150, V_High));
      B_Low_Safe := Integer'Max(1, Integer'Min(150, B_Low));
      B_High_Safe := Integer'Max(1, Integer'Min(150, B_High));
      
      -- Get values at the four corners
      Q11 := Table(V_Low_Safe, B_Low_Safe);
      Q12 := Table(V_Low_Safe, B_High_Safe);
      Q21 := Table(V_High_Safe, B_Low_Safe);
      Q22 := Table(V_High_Safe, B_High_Safe);
      
      -- Interpolate along b direction first
      R1 := Q11 * (1.0 - B_Frac) + Q12 * B_Frac;
      R2 := Q21 * (1.0 - B_Frac) + Q22 * B_Frac;
      
      -- Then interpolate along v direction
      Result := R1 * (1.0 - V_Frac) + R2 * V_Frac;
      
      return Result;
   end Bilinear_Interpolate;
   
   
   function Get_Rice_CDF(V : Float; B : Float) return Float is
      V_Idx : Float;
      B_Idx : Float;
      Result : Float;
   begin
      -- Check if values are within table bounds
      if V < V_Min or V > V_Max or B < B_Min or B > B_Max then
         -- Return boundary value or raise exception
         -- For now, clamp to boundary
         return 0.0;  -- Or handle out-of-bounds case appropriately
      end if;
      
      -- Map v and b to table indices (1-based, range 1..150)
      V_Idx := ((V - V_Min) / (V_Max - V_Min)) * 149.0 + 1.0;
      B_Idx := ((B - B_Min) / (B_Max - B_Min)) * 149.0 + 1.0;
      
      -- Check if we hit exact table values (avoid unnecessary interpolation)
      if Float'Remainder(V_Idx, 1.0) = 0.0 and Float'Remainder(B_Idx, 1.0) = 0.0 then
         -- Exact table lookup
         return Rice_Lookup(Integer(V_Idx), Integer(B_Idx));
      else
         -- Bilinear interpolation
         return Bilinear_Interpolate(V_Idx, B_Idx, Rice_Lookup);
      end if;
   end Get_Rice_CDF;

end Rice_Distribution;

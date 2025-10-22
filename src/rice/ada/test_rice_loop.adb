
with Ada.Float_Text_IO;
with Ada.Text_IO;
with Rice_Distribution;

procedure Test_Rice_Loop is
    use Ada.Float_Text_IO;
    use Ada.Text_IO;
    use Rice_Distribution;
    
    -- Single variable to control number of points for both v and b
    num_points : constant Integer := 20;
    
    v_start : constant Float := 2.0;
    v_end   : constant Float := 150.0;
    
    b_start : constant Float := 48.6846;
    b_end   : constant Float := 150.0;
    
    v_current : Float;
    b_current : Float;
    v_step_size : Float;
    b_step_size : Float;
    result : Float;
    
    -- File handle for output
    Output_File : File_Type;
    
begin
    -- Open the output file for writing
    Create(Output_File, Out_File, "rice_cdf_results.dat");
    
    -- Calculate step sizes based on num_points
    v_step_size := (v_end - v_start) / Float(num_points - 1);
    b_step_size := (b_end - b_start) / Float(num_points - 1);
    
    -- Loop through v values
    for v_idx in 0 .. num_points - 1 loop
        v_current := v_start + Float(v_idx) * v_step_size;
        
        -- Loop through b values
        for b_idx in 0 .. num_points - 1 loop
            b_current := b_start + Float(b_idx) * b_step_size;
            
            -- Call Get_Rice_CDF
            result := Get_Rice_CDF(v_current, b_current);
            
            -- Write in format: C(v_value, b_value) = result
            Put(Output_File, "C(");
            Put(Output_File, v_current, Fore => 1, Aft => 4, Exp => 0);
            Put(Output_File, ", ");
            Put(Output_File, b_current, Fore => 1, Aft => 4, Exp => 0);
            Put(Output_File, ") = ");
            Put(Output_File, result, Fore => 1, Aft => 4, Exp => 0);
            New_Line(Output_File);
        end loop;
    end loop;
    
    -- Close the output file
    Close(Output_File);
end Test_Rice_Loop;

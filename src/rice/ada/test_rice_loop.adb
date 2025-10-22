with Ada.Float_Text_IO;
with Ada.Text_IO;
with Rice_Distribution;

procedure Test_Rice_Loop is
    use Ada.Float_Text_IO;
    use Ada.Text_IO;
    use Rice_Distribution;
    
    v_start : constant Float := 2.0;
    v_end   : constant Float := 150.0;
    v_steps : constant Integer := 150;
    
    b_start : constant Float := 48.6846;
    b_end   : constant Float := 150.0;
    b_steps : constant Integer := 150;
    
    v_current : Float;
    b_current : Float;
    v_step_size : Float;
    b_step_size : Float;
    result : Float;
    
begin
    -- Calculate step sizes
    v_step_size := (v_end - v_start) / Float(v_steps - 1);
    b_step_size := (b_end - b_start) / Float(b_steps - 1);
    
    Put_Line("Starting Rice CDF evaluation loop...");
    New_Line;
    
    -- Loop through v values
    for v_idx in 0 .. v_steps - 1 loop
        v_current := v_start + Float(v_idx) * v_step_size;
        
        -- Loop through b values
        for b_idx in 0 .. b_steps - 1 loop
            b_current := b_start + Float(b_idx) * b_step_size;
            
            -- Call Get_Rice_CDF
            result := Get_Rice_CDF(v_current, b_current);
            
            -- Optionally print results (you may want to comment this out for all 22,500 iterations)
            -- Put("v="); Put(v_current, Fore => 1, Aft => 2, Exp => 0);
            -- Put(", b="); Put(b_current, Fore => 1, Aft => 2, Exp => 0);
            -- Put(", CDF="); Put(result, Fore => 1, Aft => 6, Exp => 0);
            -- New_Line;
        end loop;
        
        -- Progress indicator every 10 v values
        if v_idx mod 10 = 0 then
            Put("Progress: ");
            Put(Float(v_idx) / Float(v_steps) * 100.0, Fore => 1, Aft => 1, Exp => 0);
            Put_Line("%");
        end if;
    end loop;
    
    Put_Line("Completed all evaluations!");
end Test_Rice_Loop;

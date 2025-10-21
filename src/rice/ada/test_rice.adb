
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Float_Text_IO;     use Ada.Float_Text_IO;
with Rice_Distribution;     use Rice_Distribution;

procedure Test_Rice is
   v_input      : Float;
   b_input      : Float;
   result       : Float;
   choice       : Character;
   continue     : Boolean := True;
   
begin
   Put_Line("=========================================");
   Put_Line("  Rice Distribution CDF Test Program");
   Put_Line("=========================================");
   New_Line;
   
   Put_Line("Table parameters:");
   Put("  v range: ");
   Put(v_min, Fore => 1, Aft => 2, Exp => 0);
   Put(" to ");
   Put(v_max, Fore => 1, Aft => 2, Exp => 0);
   New_Line;
   
   Put("  b range: ");
   Put(b_min, Fore => 1, Aft => 2, Exp => 0);
   Put(" to ");
   Put(b_max, Fore => 1, Aft => 2, Exp => 0);
   New_Line;
   
   Put("  sigma: ");
   Put(sigma, Fore => 1, Aft => 2, Exp => 0);
   New_Line;
   New_Line;
   
   while continue loop
      Put_Line("Enter test values:");
      Put("  v (non-centrality parameter): ");
      Get(v_input);
      
      Put("  b (evaluation point): ");
      Get(b_input);
      
      New_Line;
      Put_Line("Computing Rice CDF...");
      
      result := Get_Rice_CDF(v_input, b_input);
      
      Put_Line("----------------------------------------");
      Put("v = ");
      Put(v_input, Fore => 1, Aft => 4, Exp => 0);
      New_Line;
      
      Put("b = ");
      Put(b_input, Fore => 1, Aft => 4, Exp => 0);
      New_Line;
      
      Put("CDF(v, b) = ");
      Put(result, Fore => 1, Aft => 6, Exp => 0);
      New_Line;
      Put_Line("----------------------------------------");
      New_Line;
      
      -- Check if values are in bounds
      if v_input < v_min or v_input > v_max then
         Put_Line("WARNING: v is outside table bounds!");
      end if;
      
      if b_input < b_min or b_input > b_max then
         Put_Line("WARNING: b is outside table bounds!");
      end if;
      
      New_Line;
      Put("Continue testing? (y/n): ");
      Get(choice);
      Skip_Line;
      
      continue := (choice = 'y' or choice = 'Y');
      New_Line;
   end loop;
   
   Put_Line("Exiting test program.");
   
end Test_Rice;

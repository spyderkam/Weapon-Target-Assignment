with Ada.Float_Text_IO;
with Ada.Text_IO;
with Rice_Distribution;

procedure Test_Rice_Simple is
    use Ada.Float_Text_IO;
    use Ada.Text_IO;
    use Rice_Distribution;
    
    v      : Float := 10.0;
    b      : Float := 75.0;
    result : Float;
begin
    result := Get_Rice_CDF(v, b);
    
    Put("Value at (v=");
    Put(v, Fore => 1, Aft => 1, Exp => 0);
    Put(", b=");
    Put(b, Fore => 1, Aft => 1, Exp => 0);
    Put(") = ");
    Put(result, Fore => 1, Aft => 6, Exp => 0);
    New_Line;
end Test_Rice_Simple;

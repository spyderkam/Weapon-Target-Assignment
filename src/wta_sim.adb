with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Strings.Fixed;
with Ada.Text_IO;

procedure weapon_target_assignment is
    package Float_Elementary_Functions is new Ada.Numerics.Generic_Elementary_Functions(Float);

    use Ada.Strings;
    use Ada.Text_IO;  -- Include this line to use Put_Line directly
    use Float_Elementary_Functions;

    type Weapon is record
        x, y, z      : Float;
        beta         : Float;    -- intrinsic lethality
        R_max        : Float;    -- max effective range
        C_available  : Natural;  -- ammunition count
    end record;

    type Target is record
        x, y, z      : Float;
        vx, vy, vz   : Float;    -- velocity components
        A            : Float;    -- armor effectiveness
        S            : Float;    -- structural integrity
        rho          : Float;    -- critical component density
    end record;

    -- Calculate distance between weapon i and target j
    function Distance(w: Weapon; t: Target) return Float is
    begin
        return Sqrt((w.x - t.x)**2 + (w.y - t.y)**2 + (w.z - t.z)**2);
    end Distance;

    -- Check if target is in range
    function In_Range(w: Weapon; t: Target) return Boolean is
    begin
        return Distance(w, t) <= w.R_max;
    end In_Range;

    -- $\alpha_{\imath}{\jmath}$
    function Vulnerability_Coefficient(w: Weapon; t: Target) return Float is
    begin
     	return (w.beta/(t.A*t.S)) * t.rho;  -- Eventually need to replace rho with f(rho).
    end Vulnerability_Coefficient;

begin
    declare
        weap1   : Weapon := (x => 0.0, y => 0.0, z => 0.0, beta => 1.0, R_max => 100.0, C_available => 10);
        targ1   : Target := (x => 50.0, y => 30.0, z => 0.0, vx => 0.0, vy => 0.0, vz => 0.0, A => 2.0, S => 1.5, rho => 0.8);
        dist    : Float;
        alpha11 : Float;
    begin
        dist := Distance(weap1, targ1);
        alpha11 := Vulnerability_Coefficient(weap1, targ1);
        Put_Line(Fixed.Trim("Weapon 1 to Target 1 distance:" & Float'Image(dist) & " m", Left));
        Put_Line(Fixed.Trim("Target in range: " & Boolean'Image(In_Range(weap1, targ1)), Left));
        Put_Line("Vulnerability Coefficient:" & Float'Image(alpha11));
    end;
end weapon_target_assignment;


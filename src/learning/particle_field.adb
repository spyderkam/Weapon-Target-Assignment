with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Strings.Fixed;
with Ada.Text_IO;

procedure test is
    package Float_Elementary_Functions is new Ada.Numerics.Generic_Elementary_Functions(Float);

    use Ada.Strings;
    use Ada.Text_IO;  -- Include this line to use Put_Line directly
    use Float_Elementary_Functions;

    type Particle is record
        x           : Float;
        y           : Float;
        fieldRadius : Float;
        fieldArea   : Float;
    end record;

    procedure Initialize_Particle(p: out Particle; x, y, fieldRadius: Float) is
    begin
        p.x := x;
        p.y := y;
        p.fieldRadius := fieldRadius;
        p.fieldArea := 3.14159*fieldRadius**2;
    end Initialize_Particle;

    procedure Move(p: in out Particle; dx, dy: Float) is
    begin
        p.x := p.x + dx;
        p.y := p.y + dy;
    end Move;

    procedure InField(p: in Particle; other_particle: Particle) is
        distance: Float;
    begin
        distance := Sqrt((p.x - other_particle.x)**2 + (p.y - other_particle.y)**2);
        if distance <= other_particle.fieldRadius then
            Put_Line(Fixed.Trim("Particle affected by other particle's field", Left));
        else
            Put_Line(Fixed.Trim("Particle not affected by other particle's field", Left));
        end if;
    end InField;

begin
    declare
        part0, part1 : Particle;
    begin
        -- Initialize particles, Python style.
        Initialize_Particle(part0, 0.0, 0.0, 1.0);
        Initialize_Particle(part1, 3.0, 4.0, 10.0);
        
        InField(part0, part1);
    end;
end test;

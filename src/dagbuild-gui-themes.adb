
package body DAGBuild.GUI.Themes is

    function Hex_Color(c : Raw_Color) return SDL.Video.Palettes.Colour is
        use Interfaces;
        use SDL.Video.Palettes;

        ret : SDL.Video.Palettes.Colour;
    begin
        -- if greater than 16#FFFFFF# then we have an alpha component,
        -- otherwise assume opaque
        if c > 16#FFFFFF# then
            ret.Red     := Colour_Component(Shift_Right(c and 16#FF_00_00_00#, 24));
            ret.Green   := Colour_Component(Shift_Right(c and 16#00_FF_00_00#, 16));
            ret.Blue    := Colour_Component(Shift_Right(c and 16#00_00_FF_00#, 8));
            ret.Alpha   := Colour_Component(c and 16#FF#);
        else
            ret.Red     := Colour_Component(Shift_Right(c and 16#FF_00_00#, 16));
            ret.Green   := Colour_Component(Shift_Right(c and 16#00_FF_00#, 8));
            ret.Blue    := Colour_Component(c and 16#FF#);
            ret.Alpha   := Colour_Component(16#FF#);
        end if;

        return ret;
    end Hex_Color;

end DAGBuild.GUI.Themes;
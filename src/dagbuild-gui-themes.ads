with Interfaces;

with SDL.Video.Palettes;

package DAGBuild.GUI.Themes is
    
    type Raw_Color is new Interfaces.Unsigned_32;

    function Hex_Color(c : Raw_Color) return SDL.Video.Palettes.Colour;

end DAGBuild.GUI.Themes;
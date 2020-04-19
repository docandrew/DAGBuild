with Ada.Strings.Unbounded;

with SDL.TTFs;

with DAGBuild.GUI.State;

package DAGBuild.GUI.Widgets is

    DAG_Font        : SDL.TTFs.Fonts;

    -- Draw a button, return True if clicked
    function Button(st      : in out DAGBuild.GUI.State.UIState;
                    x       : SDL.Natural_Coordinate;
                    y       : SDL.Natural_Coordinate;
                    Label   : String := "") return Boolean;

    -- Draw a slider, return True if value was updated
    -- @param Max is the maximum value this slider can represent
    -- @param Val is the underlying value to be changed/set
    -- @return True if value was changed
    function Slider(st  : in out DAGBuild.GUI.State.UIState;
                    x   : SDL.Natural_Coordinate;
                    y   : SDL.Natural_Coordinate;
                    Max : Integer;
                    Val : in out Integer) return Boolean;

    -- Draw a label
    procedure Label(st      : in out DAGBuild.GUI.State.UIState;
                    Text    : String;
                    x       : SDL.Natural_Coordinate;
                    y       : SDL.Natural_Coordinate);

end DAGBuild.GUI.Widgets;

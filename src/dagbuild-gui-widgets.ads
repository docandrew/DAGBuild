
with DAGBuild.GUI.State;

package DAGBuild.GUI.Widgets is

    -- Draw a button, return True if clicked
    function Button(st : in out DAGBuild.GUI.State.UIState;
                    x  : SDL.Natural_Coordinate;
                    y  : SDL.Natural_Coordinate) return Boolean;

    -- @param Max is the maximum value this slider can represent
    -- @param Val is the underlying value to be changed/set
    -- @return True if value was changed
    function Slider(st  : in out DAGBuild.GUI.State.UIState;
                    x   : SDL.Natural_Coordinate;
                    y   : SDL.Natural_Coordinate;
                    Max : Integer;
                    Val : in out Integer) return Boolean;

end DAGBuild.GUI.Widgets;

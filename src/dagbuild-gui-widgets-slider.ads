
package DAGBuild.GUI.Widgets.Slider is

    -- Draw a vertical slider, return True if value was updated
    -- @param Max is the maximum value this slider can represent
    -- @param Val is the underlying value to be changed/set
    -- @return True if value was changed
    function Vertical_Slider (st  : in out DAGBuild.GUI.State.UIState;
                              x   : SDL.Natural_Coordinate;
                              y   : SDL.Natural_Coordinate;
                              Max : Integer;
                              Val : in out Integer) return Boolean;

    -- Draw a horizontal slider, return True if value was updated
    -- @param Max is the maximum value this slider can represent
    -- @param Val is the underlying value to be changed/set
    -- @return True if value was changed
    function Horizontal_Slider (st  : in out DAGBuild.GUI.State.UIState;
                                x   : SDL.Natural_Coordinate;
                                y   : SDL.Natural_Coordinate;
                                Max : Integer;
                                Val : in out Integer) return Boolean;

end DAGBuild.GUI.Widgets.Slider;

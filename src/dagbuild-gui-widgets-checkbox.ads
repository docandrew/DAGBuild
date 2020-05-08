with SDL;

package DAGBuild.GUI.Widgets.Checkbox is

    -- Draw button with size depending on font size
    --@return True if this was just changed.
    function Checkbox (st      : in out DAGBuild.GUI.State.UIState;
                       x       : SDL.Natural_Coordinate;
                       y       : SDL.Natural_Coordinate;
                       Label   : String := "";
                       Checked : in out Boolean) return Boolean;

end DAGBuild.GUI.Widgets.Checkbox;
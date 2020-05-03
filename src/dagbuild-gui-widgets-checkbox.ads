with SDL;

package DAGBuild.GUI.Widgets.Checkbox is

    -- Draw button with size depending on font size
    procedure Checkbox (st      : in out DAGBuild.GUI.State.UIState;
                        x       : SDL.Natural_Coordinate;
                        y       : SDL.Natural_Coordinate;
                        Label   : String := "";
                        Checked : in out Boolean);

end DAGBuild.GUI.Widgets.Checkbox;
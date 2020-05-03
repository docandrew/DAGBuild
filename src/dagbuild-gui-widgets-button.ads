
with SDL;

package DAGBuild.GUI.Widgets.Button is
    -- Draw button with size depending on font size
    function Button (st             : in out DAGBuild.GUI.State.UIState;
                     x              : SDL.Natural_Coordinate;
                     y              : SDL.Natural_Coordinate;
                     Label          : String := "") return Boolean;


    -- Draw a button, return True if clicked
    function Button (st             : in out DAGBuild.GUI.State.UIState;
                     x              : SDL.Natural_Coordinate;
                     y              : SDL.Natural_Coordinate;
                     Label          : String := "";
                     Button_Width   : SDL.Natural_Coordinate;
                     Button_Height  : SDL.Natural_Coordinate) return Boolean;

end DAGBuild.GUI.Widgets.Button;
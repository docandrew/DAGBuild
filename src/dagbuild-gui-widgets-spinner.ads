with SDL;

package DAGBuild.GUI.Widgets.Spinner is

    -- Spinner or spinbox widget.
    --@return True if value was updated, False otherwise.
    generic
        type T is (<>);
    function Spinner (st    : in out DAGBuild.GUI.State.UIState;
                      x     : SDL.Natural_Coordinate;
                      y     : SDL.Natural_Coordinate;
                      Val   : in out T;
                      Min   : T := T'First;
                      Max   : T := T'Last) return Boolean;

end DAGBuild.GUI.Widgets.Spinner;
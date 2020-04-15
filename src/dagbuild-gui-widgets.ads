
with DAGBuild.GUI.State;

package DAGBuild.GUI.Widgets is

    --type Interaction is (None, Click, Rt_Click, Hover);

    function Button(st : in out DAGBuild.GUI.State.UIState;
                    x  : SDL.Natural_Coordinate;
                    y  : SDL.Natural_Coordinate) return Boolean;

end DAGBuild.GUI.Widgets;
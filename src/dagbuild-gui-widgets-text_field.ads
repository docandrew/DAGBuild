with Ada.Strings.Unbounded;
with DAGBuild.GUI.State;

package DAGBuild.GUI.Widgets.Text_Field is

    -- Single-Line Text Field
    -- @field Display_Length is the number of characters that this widget can display
    -- @field Max_Length is the max number of characters that this widget can hold
    -- @return True if we hit Return key in this field.
    function Text_Field (st              : in out DAGBuild.GUI.State.UIState;
                         Text            : in out Ada.Strings.Unbounded.Unbounded_String;
                         x               : SDL.Natural_Coordinate;
                         y               : SDL.Natural_Coordinate;
                         Display_Length  : Natural := 20;
                         Max_Length      : Natural := 20) return Boolean;

end DAGBuild.GUI.Widgets.Text_Field;
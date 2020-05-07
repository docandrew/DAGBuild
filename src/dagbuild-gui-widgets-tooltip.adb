with Ada.Strings.Unbounded;

with Interfaces.C; use Interfaces.C;

package body DAGBuild.GUI.Widgets.Tooltip is

    procedure Tooltip (st : in out DAGBuild.GUI.State.UIState)
    is
        package UBS renames Ada.Strings.Unbounded;

        --@TODO get screen dimensions to make sure we aren't rendering off-screen.
        Offset_X : constant SDL.Positive_Dimension := 16;
        Offset_Y : constant SDL.Positive_Dimension := 16;

        w : SDL.Dimension;
        h : SDL.Dimension;
    begin
        Draw_Text (r        => st.Renderer,
                   Text     => UBS.To_String (st.Tooltip),
                   x        => st.Mouse_x + Offset_X,
                   y        => st.Mouse_y + Offset_Y,
                   w        => w,
                   h        => h,
                   Color    => st.Theme.EditorSuggestWidget_highlightForeground,
                   BG_Color => st.Theme.EditorSuggestWidget_background);

        Outline_Rect (st.Renderer,
                      st.Mouse_x + Offset_X,
                      st.Mouse_y + Offset_Y,
                      w,
                      h,
                      st.Theme.EditorSuggestWidget_border);
    end Tooltip;

end DAGBuild.GUI.Widgets.Tooltip;

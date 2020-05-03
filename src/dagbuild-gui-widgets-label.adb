
with DAGBuild.GUI.Settings;

with Interfaces.C; use Interfaces.C;

package body DAGBuild.GUI.Widgets.Label is

    -- Draw a label with the given text a specific location
    procedure Label(st              : in out DAGBuild.GUI.State.UIState;
                    Text            : String;
                    x               : SDL.Natural_Coordinate;
                    y               : SDL.Natural_Coordinate;
                    Display_Length  : Natural := 10)
    is
        -- Labels have an id and scope, but we ignore them
        id      : constant DAGBuild.GUI.State.ID := DAGBuild.GUI.State.Next_ID(st);
        scope   : constant DAGBuild.GUI.State.Scope := st.Curr_Scope;
        pragma Unreferenced(id, scope);

        Field_Width     : constant SDL.Positive_Dimension := SDL.Positive_Dimension(DAGBuild.GUI.Settings.Font_Size) * 
                                                                 SDL.Positive_Dimension(Display_Length);
        Field_Height    : constant SDL.Positive_Dimension := 2 * SDL.Positive_Dimension(DAGBuild.GUI.Settings.Font_Size);
        
        -- How far inside the field to start drawing characters
        Text_Draw_Offset : constant SDL.Positive_Dimension := 4;

        w : SDL.Dimension;
        h : SDL.Dimension;
    begin
        Draw_Rect (st.Renderer,
                   x,
                   y,
                   Field_Width,
                   Field_Height,
                   st.Theme.Input_Background);

        Draw_Text (r        => st.Renderer,
                   Text     => Text, 
                   x        => x + Text_Draw_Offset,
                   y        => y + 8,
                   w        => w,
                   h        => h,
                   Color    => st.Theme.Input_Foreground,
                   BG_Color => st.Theme.Input_Background);
    end Label;

end DAGBuild.GUI.Widgets.Label;

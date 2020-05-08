with Ada.Strings.Unbounded;

with Interfaces.C;

with DAGBuild.GUI.Widgets.Button;
with DAGBuild.GUI.Widgets.Label;

package body DAGBuild.GUI.Widgets.Spinner is

    function Spinner (st    : in out DAGBuild.GUI.State.UIState;
                      x     : SDL.Natural_Coordinate;
                      y     : SDL.Natural_Coordinate;
                      Val   : in out T;
                      Min   : T := T'First;
                      Max   : T := T'Last) return Boolean
    is
        package B renames DAGBuild.GUI.Widgets.Button;
        package L renames DAGBuild.GUI.Widgets.Label;

        package UBS renames Ada.Strings.Unbounded;
        
        use Interfaces.C; -- "+" operator

        Button_Width    : SDL.Positive_Dimension := 16;
        Button_Height   : SDL.Positive_Dimension := 26;

        Minus           : Boolean := False;
        Plus            : Boolean := False;
        Text_Changed    : Boolean := False;

        --Val_As_String   : UBS.Unbounded_String := UBS.To_Unbounded_String (T'Image (Val));
    begin
        -- 2 buttons on either side of a text field.
        Minus := B.Button (st, x, y, "-", "", Button_Width, Button_Height);

        if Minus and Val /= T'First then
            Val := T'Pred (Val);
        end if;

        --@TODO make this an editable field.
        L.Label (st,
                 T'Image(Val),
                 x => x + Button_Width + 2,
                 y => y,
                 Display_Length => 5,
                 BG_Color => st.Theme.Input_Background);
        
        Plus := B.Button (st, x + 90, y, "+", "", Button_Width, Button_Height);

        if Plus and Val /= T'Last then
            Val := T'Succ (Val);
        end if;

        return Plus or Minus;
    end Spinner;                      

end DAGBuild.GUI.Widgets.Spinner;
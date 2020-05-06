with Ada.Strings.Unbounded;

with Interfaces.C;

with DAGBuild.GUI.Widgets.Button;
with DAGBuild.GUI.Widgets.Text_Field;

package body DAGBuild.GUI.Widgets.Spinner is

    function Spinner (st    : in out DAGBuild.GUI.State.UIState;
                      x     : SDL.Natural_Coordinate;
                      y     : SDL.Natural_Coordinate;
                      Val   : in out T;
                      Min   : T := T'First;
                      Max   : T := T'Last) return Boolean
    is
        package B renames DAGBuild.GUI.Widgets.Button;
        package TF renames DAGBuild.GUI.Widgets.Text_Field;

        package UBS renames Ada.Strings.Unbounded;
        
        use Interfaces.C; -- "+" operator

        Button_Width    : SDL.Positive_Dimension := 16;
        Button_Height   : SDL.Positive_Dimension := 28;

        Minus           : Boolean := False;
        Plus            : Boolean := False;
        Text_Changed    : Boolean := False;

        --@TODO will likely need a more efficient way to do this, or cache the value.
        Val_As_String   : UBS.Unbounded_String := UBS.To_Unbounded_String (T'Image (Val));
    begin
        -- 2 buttons on either side of a text field.
        Minus   := B.Button (st, x, y, "-", Button_Width, Button_Height);

        if Minus and Val /= T'First then
            Val := T'Pred (Val);
            return True;
        end if;

        Text_Changed := TF.Text_Field (st,
                                       Val_As_String,
                                       x => x + Button_Width + 2,
                                       y => y,
                                       Display_Length => 5,
                                       Max_Length => 5);

        --@TODO find a way to detect whether the Text_Field was the active item,
        -- and only perform the conversion below once it goes inactive.
        if Text_Changed then
            Try_Convert: declare
            begin
                Val := T'Value (UBS.To_String (Val_As_String));
                return True;
            exception
                when others =>
                    -- don't accept the change.
                    null;
            end Try_Convert;
        end if;                                    
        
        Plus    := B.Button (st, x + 90, y, "+", Button_Width, Button_Height);

        if Plus and Val /= T'Last then
            Val := T'Succ (Val);
            return True;
        end if;

        return False;
    end Spinner;                      

end DAGBuild.GUI.Widgets.Spinner;
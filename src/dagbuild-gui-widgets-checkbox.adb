
with DAGBuild.GUI.Settings;

with Interfaces.C; use Interfaces.C;

with SDL.Events;
with SDL.Events.Keyboards;

package body DAGBuild.GUI.Widgets.Checkbox is

    function Checkbox (st            : in out DAGBuild.GUI.State.UIState;
                       x             : SDL.Natural_Coordinate;
                       y             : SDL.Natural_Coordinate;
                       Label         : String := "";
                       Checked       : in out Boolean) return Boolean
    is
        use DAGBuild.GUI.State;

        id      : constant DAGBuild.GUI.State.ID := DAGBuild.GUI.State.Next_ID(st);
        Scope   : constant DAGBuild.GUI.State.Scope_ID := st.Curr_Scope;
        
        Dummy_w : SDL.Positive_Dimension;
        Dummy_h : SDL.Positive_Dimension;
              
        Button_Width    : constant SDL.Positive_Dimension := 16;
        Button_Height   : constant SDL.Positive_Dimension := 16;

        Offset_y        : constant SDL.Positive_Dimension := 4;

        --@TODO make this a function of font size
        Label_x : constant SDL.Positive_Dimension := x + Button_Width + 4;
        Label_y : constant SDL.Positive_Dimension := y + Offset_y;

        Preview         : Boolean := False;
        Changed         : Boolean := False;
    begin

        if Region_Hit (st, x, y + Offset_y, Button_Width, Button_Height) then
            st.Hot_Item     := id;
            st.Hot_Scope    := Scope;

            if st.Active_Item = NO_ITEM and st.Mouse_Down then
                st.Active_Item  := id;
                st.Active_Scope := Scope;
            end if;
        end if;

        -- if no widget has keyboard focus, take it
        if st.Kbd_Item = NO_ITEM then
            st.Kbd_Item     := id;
            st.Kbd_Scope    := Scope;
        end if;

        -- if we have keyboard focus, show it and update heartbeat
        if st.Kbd_Item = id and st.Kbd_Scope = Scope then
            Outline_Rect (st.Renderer,
                          x - 2,
                          y + Offset_y - 2,
                          Button_Width + 4,
                          Button_Height + 4,
                          st.Theme.InputOption_activeBorder);

            st.Kbd_Heartbeat := True;
        end if;
        
        if st.Hot_Item = id and st.Hot_Scope = Scope then
            -- Hot - highlight background
            Draw_Rect (st.Renderer,
                       x,
                       y + Offset_y,
                       Button_Width,
                       Button_Height,
                       st.Theme.Button_HoverBackground);

            if st.Active_Item = id and st.Active_Scope = Scope then
                -- Hot and Active - draw preview of the change while the
                -- mouse button is held down.
                Preview := True;
            end if;
        else
            -- Not hot, could be active. Draw normal background.
            Draw_Rect (st.Renderer,
                       x,
                       y + Offset_y,
                       Button_Width,
                       Button_Height,
                       st.Theme.Button_Background);
        end if;

        -- if checked, or previewing and not checked, then draw the inner square.
        if Checked xor Preview then
            Draw_Rect (st.Renderer,
                       x + 4,
                       y + Offset_y + 4,
                       Button_Width - 8,
                       Button_Height - 8,
                       st.Theme.Button_Foreground);
        end if;

        -- Always draw label if there is one.
        if Label'Length /= 0 then
            Draw_Text (r        => st.Renderer,
                       Text     => Label,
                       x        => Label_x,
                       y        => Label_y,
                       w        => Dummy_w,
                       h        => Dummy_h,
                       Color    => st.Theme.Input_Foreground,
                       BG_Color => (0,0,0,0)); --st.Theme.Editor_Background);
        end if;

        -- Keyboard input processing
        HandleKeys: declare
            use SDL.Events.Keyboards;
        begin
            if st.Kbd_Item = id and st.Kbd_Scope = Scope then
                case st.Kbd_Pressed is
                    when SDL.Events.Keyboards.Code_Tab =>
                        -- Lose focus, next widget will snag it.
                        st.Kbd_Item := NO_ITEM;
                        st.Kbd_Scope := NO_SCOPE;

                        -- or make previous widget get focus if we're doing shift+tab.
                        if (st.Kbd_Modifier and Modifier_Shift) /= 0 then
                            st.Kbd_Item := st.Last_Widget;
                            st.Kbd_Scope := st.Last_Scope;
                        end if;

                        -- clear key
                        st.Kbd_Pressed := NO_KEY;

                    when SDL.Events.Keyboards.Code_Return | 
                         SDL.Events.Keyboards.Code_Space =>
                        -- act like a press occurred.
                        st.Kbd_Pressed := NO_KEY;
                        Checked := not Checked;
                        Changed := True;
                    when others =>
                        null;
                end case;
            end if;
        end HandleKeys;

        st.Last_Widget := id;
        st.Last_Scope := Scope;

        -- If button is hot and active, but button not up, then it was clicked
        if st.Mouse_Down = False and 
            st.Hot_Item = id and st.Hot_Scope = Scope and
            st.Active_Item = id and st.Active_Scope = Scope then
           Checked := not Checked;
           Changed := True;
        end if;

        return Changed;
    end Checkbox;

end DAGBuild.GUI.Widgets.Checkbox;
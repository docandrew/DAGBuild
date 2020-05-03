
with SDL.Events;
with SDL.Events.Keyboards;

with Interfaces.C; use Interfaces.C;

package body DAGBuild.GUI.Widgets.Slider is

    -- function Horizontal_Slider(st  : in out DAGBuild.GUI.State.UIState;
    --                            x   : SDL.Natural_Coordinate;
    --                            y   : SDL.Natural_Coordinate;
    --                            Max : Integer;
    --                            Val : in out Integer) return Boolean
    -- is
    --     use DAGBuild.GUI.State;

    --     id      : constant DAGBuild.GUI.State.ID := DAGBuild.GUI.State.Next_ID(st);
    --     scope   : constant DAGBuild.GUI.State.Scope := st.Curr_Scope;

    --     Slider_Width    : constant SDL.Positive_Dimension := 255;
    --     Slider_Height   : constant SDL.Positive_Dimension := 12;
        
    --     Button_Width    : constant SDL.Positive_Dimension := 16;
    --     Button_Height   : constant SDL.Positive_Dimension := 16;
        
    --     xpos : constant SDL.Natural_Coordinate :=
    --         SDL.Natural_Coordinate((Integer(Slider_Width) * Val) / Max);
    -- begin

    --     if Region_Hit(st, x, y, Slider_Width, Button_Height) then
    --         st.Hot_Item := id;
    --         st.Hot_Scope := scope;

    --         if st.Active_Item = NO_ITEM and st.Mouse_Down then
    --             st.Active_Item := id;
    --             st.Active_Scope := scope;
    --         end if;
    --     end if;

    --     -- if no widget has keyboard focus, take it
    --     if st.Kbd_Item = NO_ITEM then
    --         st.Kbd_Item := id;
    --         st.Kbd_Scope := Scope;
    --     end if;

    --     -- if we have keyboard focus, show it and update heartbeat
    --     if st.Kbd_Item = id and st.Kbd_Scope = Scope then
    --         Outline_Rect(st.Renderer,
    --                     x - 2,
    --                     y - 2,
    --                     Button_Width + 4,
    --                     Slider_Height + 4,
    --                     st.Theme.InputOption_activeBorder);

    --         st.Kbd_Heartbeat := True;
    --     end if;

    --     -- Draw the scrollbar
    --     Draw_Rect (st.Renderer,
    --                x + (Button_Width - Slider_Width) / 2,
    --                y,
    --                Slider_Width,
    --                Slider_Height,
    --                st.Theme.Scrollbar_shadow);

    --     -- Draw the button
    --     if (st.Hot_Item = id and st.Hot_Scope = Scope) or 
    --         (st.Active_Item = id and st.Active_Scope = Scope) then
    --         Draw_Rect ( st.Renderer,
    --                     x,
    --                     y + ypos,
    --                     Button_Width,
    --                     Button_Height,
    --                     st.Theme.ScrollbarSlider_activeBackground);
    --     else
    --         Draw_Rect ( st.Renderer,
    --                     x,
    --                     y + ypos,
    --                     Button_Width,
    --                     Button_Height,
    --                     st.Theme.ScrollbarSlider_background);
    --     end if;

    --     -- Update slider position
    --     if st.Active_Item = id and st.Active_Scope = Scope then
    --         Update: declare
    --             Mouse_Pos : Integer := Integer(st.Mouse_y) - Integer(y);
    --             New_Val : Integer;
    --         begin

    --             if Mouse_Pos < 0 then
    --                 Mouse_Pos := 0;
    --             end if;

    --             if Mouse_Pos > 255 then
    --                 Mouse_Pos := 255;
    --             end if;

    --             New_Val := (Mouse_Pos * Max) / 255;

    --             if Val /= New_Val then
    --                 Val := New_Val;
    --                 return True;
    --             end if;
    --         end Update;
    --     end if;

    --     -- If we have keyboard focus, process keys
    --     HandleKeys: Declare
    --         use SDL.Events.Keyboards;
    --     begin    
    --         if st.Kbd_Item = id and st.Kbd_Scope = Scope then
    --             --Ada.Text_IO.Put_Line("slider key: " & st.Kbd_Pressed'Image);

    --             case st.Kbd_Pressed is
    --                 when SDL.Events.Keyboards.Code_Tab =>
    --                     -- Lose focus, next widget will snag it.
    --                     st.Kbd_Item := NO_ITEM;
    --                     st.Kbd_Scope := NO_SCOPE;

    --                     -- or make previous widget get focus if we're doing shift+tab.
    --                     if (st.Kbd_Modifier and Modifier_Shift) /= 0 then
    --                         st.Kbd_Item := st.Last_Widget;
    --                         st.Kbd_Scope := st.Last_Scope;
    --                     end if;

    --                     -- clear key
    --                     st.Kbd_Pressed := NO_KEY;

    --                 when SDL.Events.Keyboards.Code_Left =>
    --                     if Val > 0 then
    --                         Val := Val - 1;
    --                     end if;

    --                     st.Kbd_Pressed := NO_KEY;
    --                     return True;

    --                 when SDL.Events.Keyboards.Code_Right =>
    --                     if Val < Max then
    --                         Val := Val + 1;
    --                     end if;

    --                     st.Kbd_Pressed := NO_KEY;
    --                     return True;

    --                 when others =>
    --                     --Ada.Text_IO.Put_Line("other: " & st.Kbd_Pressed'Image);
    --                     null;
    --             end case;
    --         end if;
    --     end HandleKeys;

    --     st.Last_Widget := id;
    --     st.Last_Scope := Scope;

    --     return False;
    -- end Horizontal_Slider;

    function Vertical_Slider(st  : in out DAGBuild.GUI.State.UIState;
                             x   : SDL.Natural_Coordinate;
                             y   : SDL.Natural_Coordinate;
                             Max : Integer;
                             Val : in out Integer) return Boolean
    is
        use DAGBuild.GUI.State;

        id      : constant DAGBuild.GUI.State.ID := DAGBuild.GUI.State.Next_ID(st);
        scope   : constant DAGBuild.GUI.State.Scope := st.Curr_Scope;

        Slider_Width    : constant SDL.Positive_Dimension := 12;
        Slider_Height   : constant SDL.Positive_Dimension := 255;
        
        Button_Width    : constant SDL.Positive_Dimension := 16;
        Button_Height   : constant SDL.Positive_Dimension := 16;
        
        ypos : constant SDL.Natural_Coordinate :=
            SDL.Natural_Coordinate((Integer(Slider_Height - 16) * Val) / Max);
    begin

        if Region_Hit(st, x, y, Button_Width, Slider_Height) then
            st.Hot_Item := id;
            st.Hot_Scope := scope;

            if st.Active_Item = NO_ITEM and st.Mouse_Down then
                st.Active_Item := id;
                st.Active_Scope := scope;
            end if;
        end if;

        -- if no widget has keyboard focus, take it
        if st.Kbd_Item = NO_ITEM then
            st.Kbd_Item := id;
            st.Kbd_Scope := Scope;
        end if;

        -- if we have keyboard focus, show it and update heartbeat
        if st.Kbd_Item = id and st.Kbd_Scope = Scope then
            Outline_Rect(st.Renderer,
                        x - 2,
                        y - 2,
                        Button_Width + 4,
                        Slider_Height + 4,
                        st.Theme.InputOption_activeBorder);

            st.Kbd_Heartbeat := True;
        end if;

        -- Draw the scrollbar
        Draw_Rect (st.Renderer,
                   x + (Button_Width - Slider_Width) / 2,
                   y,
                   Slider_Width,
                   Slider_Height,
                   st.Theme.Scrollbar_shadow);

        -- Draw the button
        if (st.Hot_Item = id and st.Hot_Scope = Scope) or 
            (st.Active_Item = id and st.Active_Scope = Scope) then
            Draw_Rect ( st.Renderer,
                        x,
                        y + ypos,
                        Button_Width,
                        Button_Height,
                        st.Theme.ScrollbarSlider_activeBackground);
        else
            Draw_Rect ( st.Renderer,
                        x,
                        y + ypos,
                        Button_Width,
                        Button_Height,
                        st.Theme.ScrollbarSlider_background);
        end if;

        -- Update slider position
        if st.Active_Item = id and st.Active_Scope = Scope then
            Update: declare
                Mouse_Pos : Integer := Integer(st.Mouse_y) - Integer(y);
                New_Val : Integer;
            begin

                if Mouse_Pos < 0 then
                    Mouse_Pos := 0;
                end if;

                if Mouse_Pos > 255 then
                    Mouse_Pos := 255;
                end if;

                New_Val := (Mouse_Pos * Max) / 255;

                if Val /= New_Val then
                    Val := New_Val;
                    return True;
                end if;
            end Update;
        end if;

        -- If we have keyboard focus, process keys
        HandleKeys: Declare
            use SDL.Events.Keyboards;
        begin    
            if st.Kbd_Item = id and st.Kbd_Scope = Scope then
                --Ada.Text_IO.Put_Line("slider key: " & st.Kbd_Pressed'Image);

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

                    when SDL.Events.Keyboards.Code_Up =>
                        if Val > 0 then
                            Val := Val - 1;
                        end if;

                        st.Kbd_Pressed := NO_KEY;
                        return True;

                    when SDL.Events.Keyboards.Code_Down =>
                        if Val < Max then
                            Val := Val + 1;
                        end if;

                        st.Kbd_Pressed := NO_KEY;
                        return True;

                    when others =>
                        --Ada.Text_IO.Put_Line("other: " & st.Kbd_Pressed'Image);
                        null;
                end case;
            end if;
        end HandleKeys;

        st.Last_Widget := id;
        st.Last_Scope := Scope;

        return False;
    end Vertical_Slider;

end DAGBuild.GUI.Widgets.Slider;

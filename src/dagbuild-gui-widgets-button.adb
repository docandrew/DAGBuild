with Ada.Real_Time;
with Ada.Strings.Unbounded;
with DAGBuild.GUI.Settings;

with Interfaces.C; use Interfaces.C;

with SDL.Events;
with SDL.Events.Keyboards;

package body DAGBuild.GUI.Widgets.Button is

    -- Draw a button, return True if clicked, False otherwise.
    function Button (st             : in out DAGBuild.GUI.State.UIState;
                     x              : SDL.Natural_Coordinate;
                     y              : SDL.Natural_Coordinate;
                     Label          : String := "";
                     Tooltip        : String := "") return Boolean
    is
        Button_Width     : constant SDL.Positive_Dimension := SDL.Positive_Dimension(DAGBuild.GUI.Settings.Font_Size) * 
                                                                 SDL.Positive_Dimension(5);
        Button_Height    : constant SDL.Positive_Dimension := 2 * SDL.Positive_Dimension(DAGBuild.GUI.Settings.Font_Size);
    begin
        return Button(st, x, y, Label, Tooltip, Button_Width, Button_Height);
    end Button;


    -- Draw a button, return True if clicked, False otherwise
    function Button (st             : in out DAGBuild.GUI.State.UIState;
                     x              : SDL.Natural_Coordinate;
                     y              : SDL.Natural_Coordinate;
                     Label          : String := "";
                     Tooltip        : String := "";
                     Button_Width   : SDL.Positive_Dimension;
                     Button_Height  : SDL.Positive_Dimension) return Boolean
    is
        package UBS renames Ada.Strings.Unbounded;

        use Ada.Real_Time;  -- "+" operator
        use DAGBuild.GUI.State;

        id      : constant DAGBuild.GUI.State.ID := DAGBuild.GUI.State.Next_ID(st);
        Scope   : constant DAGBuild.GUI.State.Scope := st.Curr_Scope;
        
        Dummy_w : SDL.Positive_Dimension;
        Dummy_h : SDL.Positive_Dimension;
        
        Button_Shadow_Offset : constant SDL.Positive_Dimension := 2;
        
        -- Button_Width    : constant SDL.Positive_Dimension := 64;
        -- Button_Height   : constant SDL.Positive_Dimension := 40;

        --@TODO make this a function of font size
        Label_x : constant SDL.Positive_Dimension := x + 4;
        Label_y : constant SDL.Positive_Dimension := y + 4;
    begin
        --Ada.Text_IO.Put_Line("Creating new button with ID: " & id'Image & " scope: " & scope'Image);

        if Region_Hit (st, x, y, Button_Width, Button_Height) then
            st.Hot_Item     := id;
            st.Hot_Scope    := Scope;
            
            -- Handle tooltip. It won't get rendered until later in the frame.
            --@TODO make this its own function, see if st.Tooltip is already set
            --  to avoid the extra work.
            if Tooltip'Length /= 0 and Ada.Real_Time.Clock > st.Hover_Start + DAGBuild.GUI.Settings.Hover_Tooltip_Time then
                st.Tooltip := UBS.To_Unbounded_String (Tooltip);
            end if;

            if st.Active_Item = NO_ITEM and st.Mouse_Down then
                st.Active_Item  := id;
                st.Active_Scope := Scope;
            end if;
        end if;

        --Ada.Text_IO.Put_Line("Active: " & st.Active_Item'Image & " Hot: " & st.Hot_Item'Image);
        -- Keyboard Focus Handling

        -- if no widget has keyboard focus, take it
        if st.Kbd_Item = NO_ITEM then
            st.Kbd_Item     := id;
            st.Kbd_Scope    := Scope;
        end if;

        -- if we have keyboard focus, show it and update heartbeat
        if st.Kbd_Item = id and st.Kbd_Scope = Scope then
            Outline_Rect (st.Renderer,
                          x - 2,
                          y - 2,
                          Button_Width + 6,
                          Button_Height + 6,
                          st.Theme.InputOption_activeBorder);

            st.Kbd_Heartbeat := True;
        end if;
        
        -- Render a button shadow
        Draw_Rect (st.Renderer,
                   x + Button_Shadow_Offset,
                   y + Button_Shadow_Offset,
                   Button_Width,
                   Button_Height,
                   st.Theme.Widget_Shadow);
        
        if st.Hot_Item = id and st.Hot_Scope = Scope then
            if st.Active_Item = id and st.Active_Scope = Scope then
                -- Hot and Active button - depress
                Draw_Rect (st.Renderer,
                           x + 2,
                           y + 2,
                           Button_Width,
                           Button_Height,
                           st.Theme.Button_HoverBackground);

                -- Draw Label
                Draw_Text (r        => st.Renderer,
                           Text     => Label,
                           x        => Label_x + 2,
                           y        => Label_y + 2,
                           w        => Dummy_w,
                           h        => Dummy_h,
                           Color    => st.Theme.Button_Foreground,
                           BG_Color => st.Theme.Button_HoverBackground);
                --@TODO play audio of label for accessibility
            else
                -- Hot but not Active - 
                Draw_Rect(st.Renderer,
                          x,
                          y,
                          Button_Width,
                          Button_Height,
                          st.Theme.Button_HoverBackground);

                Draw_Text (r        => st.Renderer,
                           Text     => Label,
                           x        => Label_x,
                           y        => Label_y,
                           w        => Dummy_w,
                           h        => Dummy_h,
                           Color    => st.Theme.Button_Foreground,
                           BG_Color => st.Theme.Button_HoverBackground);
                --@TODO play audio of label for accessibility
            end if;
        else
            -- Not hot, could be active
            Draw_Rect (st.Renderer,
                       x,
                       y,
                       Button_Width,
                       Button_Height,
                       st.Theme.Button_Background);

            Draw_Text (r        => st.Renderer,
                       Text     => Label,
                       x        => Label_x,
                       y        => Label_y,
                       w        => Dummy_w,
                       h        => Dummy_h,
                       Color    => st.Theme.Button_Foreground,
                       BG_Color => st.Theme.Button_Background);
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

                    when SDL.Events.Keyboards.Code_Return =>
                        -- act like a press occurred.
                        st.Kbd_Pressed := NO_KEY;
                        return True;
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
           return True;
        end if;

        return False;
    end Button;

end DAGBuild.GUI.Widgets.Button;
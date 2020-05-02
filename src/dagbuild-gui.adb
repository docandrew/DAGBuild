
with Interfaces.C;

--with Ada.Real_Time;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Events.Mice;
with SDL.Hints;
with SDL.Inputs.Keyboards;
with SDL.Inputs.Mice.Cursors;
with SDL.TTFs.Makers;
with SDL.Video.Palettes;
with SDL.Video.Renderers;
with SDL.Video.Renderers.Makers;

with DAGBuild.GUI.State;
with DAGBuild.GUI.Widgets;
with DAGBuild.GUI.Widgets.Button; use DAGBuild.GUI.Widgets.Button;
with DAGBuild.GUI.Widgets.Label; use DAGBuild.GUI.Widgets.Label;
with DAGBuild.GUI.Widgets.Slider; use DAGBuild.GUI.Widgets.Slider;
with DAGBuild.GUI.Widgets.Text_Field; use DAGBuild.GUI.Widgets.Text_Field;
with DAGBuild.Settings;

pragma Wide_Character_Encoding(UTF8);

package body DAGBuild.GUI is

    -- Start a round of widget drawing/interaction
    procedure IMGUI_Start(st : in out DAGBuild.GUI.State.UIState)
    is
    begin
        st.Hot_Item := DAGBuild.GUI.State.NO_ITEM;
        st.Last_IDs := (others => DAGBuild.GUI.State.NO_ITEM);

        -- Enter the first scope
        DAGBuild.GUI.State.Enter_Scope(st);
    end IMGUI_Start;

    -- Finish a round
    procedure IMGUI_Finish (st : in out DAGBuild.GUI.State.UIState)
    is
        use Ada.Real_Time;
        use DAGBuild.GUI.State;
        Now : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
    begin
        -- Handle timing
        if Now > st.Last_Blink + DAGBuild.Settings.Cursor_Blink_Rate then
            st.Blink_On := not (st.Blink_On);
            st.Last_Blink := Now;
        end if;

        -- If mouse isn't down, clear the active item.
        -- If mouse is clicked with no widget active, mark no active item (-1) so we
        -- don't "click" on something if we release the button on top of another
        -- widget.
        if st.Mouse_Down = False then
            --Ada.Text_IO.Put_Line("Setting active to NO_ITEM");
            st.Active_Item := NO_ITEM;
        else
            if st.Active_Item = NO_ITEM then
                --Ada.Text_IO.Put_Line("Setting active to INVALID_ITEM");
                st.Active_Item := INVALID_ITEM;
            end if;
        end if;

        st.Double_Click := False;

        -- If we finished a round and the heartbeat wasn't updated because a
        -- widget wasn't drawn, then remove the focus. If we don't do this, the
        -- hidden widget will keep the focus forever.
        if st.Kbd_Heartbeat = False then
            st.Kbd_Item := NO_ITEM;
            st.Kbd_Scope := NO_SCOPE;
        end if;

        -- Reset heartbeat, pressed key and any text inputs that took place
        st.Kbd_Heartbeat := False;
        st.Kbd_Pressed := NO_KEY;
        --st.Kbd_Char := ASCII.NUL;
        st.Kbd_Text := Ada.Strings.Unbounded.Null_Unbounded_String;

        -- Reset cursor if necessary. We don't want to switch back and forth
        -- between an arrow and "I-Beam" if the hot item is going to remain
        -- a text field. If we leave here with no hot item (meaning that the
        -- mouse left a text field if it was there previously), then we'll
        -- switch back to the arrow cursor. If it remains hot, we'll leave it
        -- to whatever it was set to by the widget that may have set it last.
        if st.Hot_Item = NO_ITEM then
            SDL.Inputs.Mice.Cursors.Set_Cursor(DAGBuild.GUI.State.Arrow_Cursor);
        end if;

        DAGBuild.GUI.State.Exit_Scope(st);

        -- when we finish, we should be back to the initial scope
        if st.Curr_Scope /= NO_SCOPE then
            raise DAGBuild.GUI.State.Invalid_Scope_Exception with "Called IMGUI_Finish with unclosed scopes";
        end if;
    end IMGUI_Finish;

    -- Clear Screen
    procedure Clear_Window(r : in out SDL.Video.Renderers.Renderer;
                           c : SDL.Video.Palettes.Colour)
    is
    begin
        r.Set_Draw_Colour(c);
        r.Clear;
    end Clear_Window;

    -- statics for UI demo
    Show_Button : Boolean := False;
    Red         : SDL.Video.Palettes.Colour_Component := 0;
    Green       : SDL.Video.Palettes.Colour_Component := 0;
    Blue        : SDL.Video.Palettes.Colour_Component := 0;
    Clear_Color : SDL.Video.Palettes.Colour := DAGBuild.Settings.Default_Dark.Editor_background;
    My_Str      : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String("Input here");
    My_Str2     : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String("Or here");
    Click       : Boolean := False;

    -- Render screen elements
    procedure Render(st : in out DAGBuild.GUI.State.UIState)
    is
        package Widgets renames DAGBuild.GUI.Widgets;
        use Ada.Real_Time;   -- "+" operator

        -- For buttery-smooth frame rate
        Next_Period : constant Ada.Real_Time.Time := Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds(Delay_Period);
    begin
        Clear_Window(st.Renderer, Clear_Color);

        IMGUI_Start(st);

        --Window_Surface.Blit(Source => Text_Surface);
        --Window.Update_Surface;

        Click := Button (st,
                         50,
                         50,
                         "One");

        if Click then
            Clear_Color := (255, 0, 0, 255);
            Show_Button := True;
        end if;

        if Show_Button then
            DAGBuild.GUI.State.Enter_Scope(st);
                Click := Button(st,
                                250,
                                250,
                                "Hide Me");

                if Click then
                    Clear_Color := st.Theme.InputValidation_errorBackground;
                    Show_Button := False;
                end if;

                --Hidden_Button := (if Click then False else True);
            DAGBuild.GUI.State.Exit_Scope(st);
        end if;

        Click := Button (st,
                         150,
                         50,
                         "Two");

        if Click then
            Clear_Color := st.Theme.InputValidation_infoBackground;
        end if;

        Click := Button (st,
                         50,
                         150,
                         "Three");

        if Click then
            Clear_Color := st.Theme.InputValidation_warningBackground;
        end if;

        Click := Button (st,
                         150,
                         150,
                         "Quit");
        if Click then
            st.Done := True;
        end if;

        --Slider test
        if Vertical_Slider(st, 500, 40, 255, Integer(Red)) then
            Clear_Color := (Red, Green, Blue, 255);
        end if;

        if Vertical_Slider(st, 550, 40, 255, Integer(Green)) then
            Clear_Color := (Red, Green, Blue, 255);
        end if;

        if Vertical_Slider(st, 600, 40, 255, Integer(Blue)) then
            Clear_Color := (Red, Green, Blue, 255);
        end if;

        Label(st, "Hello DAGBuild!", 50, 300);

        Click := Text_Field(st, My_Str, 50, 350, 20, 40);
        Click := Text_Field(st, My_Str2, 50, 400, 20, 40);
        
        IMGUI_Finish(st);

        st.Renderer.Present;

        --@TODO if no activity, go into a "sleep" mode, keep the display
        -- cached, but don't redraw widgets.
        delay until Next_Period;
        --st.ms_Ticks := st.ms_Ticks + Delay_Period;
    end Render;

    procedure Handle_Inputs(st : in out DAGBuild.GUI.State.UIState)
    is
        use Ada.Real_Time;  -- "+" operator on Clock
        use type SDL.Events.Mice.Buttons;
        
        Event : SDL.Events.Events.Events;
    begin
        while SDL.Events.Events.Poll(Event) loop

            case Event.Common.Event_Type is

                when SDL.Events.Mice.Motion =>
                    st.mouse_x := Event.Mouse_Motion.X;
                    st.mouse_y := Event.Mouse_Motion.Y;
                
                when SDL.Events.Mice.Button_Down =>
                    if Event.Mouse_Button.Button = SDL.Events.Mice.Left then
                        st.Mouse_Down := True;
                        
                        -- two mouse-down events within the double-click threshold
                        if Ada.Real_Time.Clock <= st.Last_Click + DAGBuild.Settings.Double_Click_Threshold then
                            st.Double_Click := True;
                        end if;

                        st.Last_Click := Ada.Real_Time.Clock;
                    end if;

                when SDL.Events.Mice.Button_Up =>
                    if Event.Mouse_Button.Button = SDL.Events.Mice.Left then
                        st.Mouse_Down := False;
                        st.Word_Select := False;
                    end if;

                when SDL.Events.Quit =>
                    st.Done := True;
                
                when SDL.Events.Keyboards.Key_Down =>
                    st.Kbd_Pressed := Event.Keyboard.Key_Sym.Key_Code;
                    st.Kbd_Modifier := Event.Keyboard.Key_Sym.Modifiers;
                    --st.Kbd_Char := ASCII.NUL;
                    -- Ada.Text_IO.Put_Line("key: " & st.Kbd_Pressed'Image);
                    -- Ada.Text_IO.Put_Line("Shift: " & SDL.Events.Keyboards.Modifier_Shift'Image);
            
                when SDL.Events.Keyboards.Key_Up =>
                    
                    case Event.Keyboard.Key_Sym.Key_Code is
                        when SDL.Events.Keyboards.Code_Escape =>
                            st.Done := True;
                        when others =>
                            null;
                    end case;

                when SDL.Events.Keyboards.Text_Input =>
                    --if(Event.Keyboard.Key_Sym.Modifiers )
                    --Ada.Text_IO.Put_Line("text: " & Interfaces.C.To_Ada(Event.Text_Input.Text));
                    st.Kbd_Text := To_Unbounded_String (Interfaces.C.To_Ada (Event.Text_Input.Text));
                when others =>
                    null;
            end case;

        end loop;
        
    end Handle_Inputs;

    -- Main rendering and input handling loop
    procedure Event_Loop (Window : in out SDL.Video.Windows.Window)
    is
        GUI_State       : DAGBuild.GUI.State.UIState;
        
        --use SDL.Inputs.Mice;
    begin
        SDL.Hints.Set (SDL.Hints.Render_Scale_Quality, "1");

        -- Create hardware renderer if available
        SDL.Video.Renderers.Makers.Create (GUI_State.Renderer, Window);

        -- Create a cursor for the text fields
        SDL.Inputs.Mice.Cursors.Create_System_Cursor (Self  => DAGBuild.GUI.State.Arrow_Cursor,
                                              Cursor_Name   => SDL.Inputs.Mice.Cursors.Arrow);

        SDL.Inputs.Mice.Cursors.Create_System_Cursor (Self  => DAGBuild.GUI.State.Text_Cursor,
                                              Cursor_Name   => SDL.Inputs.Mice.Cursors.I_Beam);

        -- Load Font used by widgets
        SDL.TTFs.Makers.Create (Font        => DAGBuild.GUI.Widgets.DAG_Font,
                                File_Name   => DAGBuild.Settings.Font_Name,
                                Point_Size  => DAGBuild.Settings.Font_Size);
        
        SDL.Inputs.Keyboards.Start_Text_Input;
        
        loop
            Render (GUI_State);
            Handle_Inputs (GUI_State);

            exit when GUI_State.Done;
        end loop;

        DAGBuild.GUI.Widgets.DAG_Font.Finalize;

    end Event_Loop;

end DAGBuild.GUI;
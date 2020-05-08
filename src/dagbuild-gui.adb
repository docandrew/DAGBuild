
with Interfaces.C;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Events.Mice;
with SDL.Hints;
with SDL.Inputs.Keyboards;
with SDL.Inputs.Mice.Cursors;
with SDL.TTFs.Makers;
with SDL.Video.Renderers;
with SDL.Video.Renderers.Makers;

with DAGBuild.GUI.State;
with DAGBuild.GUI.Settings;

pragma Wide_Character_Encoding(UTF8);

package body DAGBuild.GUI is

    -- Start a round of widget drawing/interaction
    procedure Start_Render(st : in out DAGBuild.GUI.State.UIState)
    is
    begin
        st.Hot_Item := DAGBuild.GUI.State.NO_ITEM;
        st.Last_IDs := (others => DAGBuild.GUI.State.NO_ITEM);

        -- Enter the first scope
        DAGBuild.GUI.State.Enter_Scope(st);
    end Start_Render;

    -- Finish a round
    procedure Finish_Render (st : in out DAGBuild.GUI.State.UIState)
    is
        use Ada.Real_Time;
        use DAGBuild.GUI.State;
        Now : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
    begin
        -- Handle timing
        if Now > st.Last_Blink + DAGBuild.GUI.Settings.Cursor_Blink_Rate then
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

        -- Update the underlying window surface
        st.Renderer.Present;
    end Finish_Render;


    procedure Clear_Window(st : in out DAGBuild.GUI.State.UIState;
                           c : SDL.Video.Palettes.Colour)
    is
    begin
        st.Renderer.Set_Draw_Colour(c);
        st.Renderer.Clear;
    end Clear_Window;


    -- Poll for input events and place them in the UI state.
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
                    st.Hover_Start := Ada.Real_Time.Clock;
                    st.Tooltip := Ada.Strings.Unbounded.To_Unbounded_String ("");
                
                when SDL.Events.Mice.Button_Down =>
                    if Event.Mouse_Button.Button = SDL.Events.Mice.Left then
                        st.Mouse_Down := True;
                        
                        -- two mouse-down events within the double-click threshold
                        if Ada.Real_Time.Clock <= st.Last_Click + DAGBuild.GUI.Settings.Double_Click_Threshold then
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

    --@TODO if no activity, go into a "sleep" mode, listen for events
    -- but don't redraw.
    procedure Event_Loop (Window : in out SDL.Video.Windows.Window;
                          Render : access procedure (st : in out DAGBuild.GUI.State.UIState))
    is
        use Ada.Real_Time;   -- "+" operator
        
        GUI_State       : DAGBuild.GUI.State.UIState;
        Next_Period     : Ada.Real_Time.Time;
    begin
        SDL.Hints.Set (SDL.Hints.Render_Scale_Quality, "1");

        -- Create hardware renderer if available
        SDL.Video.Renderers.Makers.Create (GUI_State.Renderer, Window);
        SDL.Video.Renderers.Set_Blend_Mode (GUI_State.Renderer, SDL.Video.Alpha_Blend);

        -- Create a cursor for the text fields
        SDL.Inputs.Mice.Cursors.Create_System_Cursor (Self  => DAGBuild.GUI.State.Arrow_Cursor,
                                              Cursor_Name   => SDL.Inputs.Mice.Cursors.Arrow);

        SDL.Inputs.Mice.Cursors.Create_System_Cursor (Self  => DAGBuild.GUI.State.Text_Cursor,
                                              Cursor_Name   => SDL.Inputs.Mice.Cursors.I_Beam);

        -- Load Font used by widgets
        SDL.TTFs.Makers.Create (Font        => DAGBuild.GUI.DAG_Font,
                                File_Name   => DAGBuild.GUI.Settings.Font_Name,
                                Point_Size  => DAGBuild.GUI.Settings.Font_Size);
        
        SDL.Inputs.Keyboards.Start_Text_Input;
        
        loop
            Next_Period := Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds(Delay_Period);
            
            DAGBuild.GUI.Start_Render (GUI_State);
            Render (GUI_State);
            DAGBuild.GUI.Finish_Render (GUI_State);
            delay until Next_Period;    -- for buttery-smooth frame rate

            Handle_Inputs (GUI_State);

            exit when GUI_State.Done;
        end loop;

        DAG_Font.Finalize;

    end Event_Loop;

end DAGBuild.GUI;

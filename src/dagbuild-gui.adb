
-- with Interfaces.C; use Interfaces.C;
with Ada.Text_IO;

with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Events.Mice;

with SDL.Video.Palettes;
with SDL.Video.Renderers;
with SDL.Video.Renderers.Makers;

with DAGBuild.GUI.State;
with DAGBuild.GUI.Widgets;

with DAGBuild.Settings;

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
    -- If mouse isn't down, clear the active item.
    -- If mouse is clicked with no widget active, mark no active item (-1) so we
    -- don't "click" on something if we release the button on top of another
    -- widget.
    procedure IMGUI_Finish(st : in out DAGBuild.GUI.State.UIState)
    is
        use DAGBuild.GUI.State;
    begin
        if st.Mouse_Down = False then
            --Ada.Text_IO.Put_Line("Setting active to NO_ITEM");
            st.Active_Item := NO_ITEM;
        else
            if st.Active_Item = NO_ITEM then
                --Ada.Text_IO.Put_Line("Setting active to INVALID_ITEM");
                st.Active_Item := INVALID_ITEM;
            end if;
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

    Show_Button : Boolean := False;

    -- Render screen elements
    procedure Render(st : in out DAGBuild.GUI.State.UIState)
    is
        Click : Boolean := False;
    begin
        Clear_Window(st.Renderer, DAGBuild.Settings.Dark_BG);

        IMGUI_Start(st);

        Click := DAGBuild.GUI.Widgets.Button(st,
                                             50,
                                             50);

        if Click then
            DAGBuild.Settings.Dark_BG := (255, 0, 0, 255);
            Show_Button := True;
        end if;

        if Show_Button then
            DAGBuild.GUI.State.Enter_Scope(st);
                Click := DAGBuild.GUI.Widgets.Button(st,
                                                     250,
                                                     250);

                if Click then
                    DAGBuild.Settings.Dark_BG := (255, 255, 0, 255);
                    Show_Button := False;
                end if;

                --Hidden_Button := (if Click then False else True);
            DAGBuild.GUI.State.Exit_Scope(st);
        end if;

        Click := DAGBuild.GUI.Widgets.Button(st,
                                             150,
                                             50);

        if Click then
            DAGBuild.Settings.Dark_BG := (255, 0, 255, 255);
        end if;

        Click := DAGBuild.GUI.Widgets.Button(st,
                                             50,
                                             150);

        if Click then
            DAGBuild.Settings.Dark_BG := (0, 255, 255, 255);
        end if;

        Click := DAGBuild.GUI.Widgets.Button(st,
                                             150,
                                             150);
        if Click then
            st.Done := True;
        end if;
        
        IMGUI_Finish(st);

        st.Renderer.Present;
        delay 0.01; -- TODO: make a "delay until"
    end Render;

    procedure Handle_Inputs(st : in out DAGBuild.GUI.State.UIState)
    is
        use type SDL.Events.Mice.Buttons;

        Event       : SDL.Events.Events.Events;
    begin
        while SDL.Events.Events.Poll(Event) loop

            case Event.Common.Event_Type is

                when SDL.Events.Mice.Motion =>
                    st.mouse_x := Event.Mouse_Motion.X;
                    st.mouse_y := Event.Mouse_Motion.Y;
                
                when SDL.Events.Mice.Button_Down =>
                    if Event.Mouse_Button.Button = SDL.Events.Mice.Left then
                        st.Mouse_Down := True;
                    end if;

                when SDL.Events.Mice.Button_Up =>
                    if Event.Mouse_Button.Button = SDL.Events.Mice.Left then
                        st.Mouse_Down := False;
                    end if;

                when SDL.Events.Quit =>
                    st.Done := True;
            
                when SDL.Events.Keyboards.Key_Up =>
                    
                    case Event.Keyboard.Key_Sym.Key_Code is
                        when SDL.Events.Keyboards.Code_Escape =>
                            st.Done := True;
                        when others =>
                            null;
                    end case;
                
                when others =>
                    null;
            end case;

        end loop;
        
    end Handle_Inputs;

    procedure Event_Loop(Window : in out SDL.Video.Windows.Window)
    is
        GUI_State   : DAGBuild.GUI.State.UIState;       
    begin
        -- Create hardware renderer if available
        SDL.Video.Renderers.Makers.Create(GUI_State.Renderer, Window);

        loop
            Render(GUI_State);
            Handle_Inputs(GUI_State);

            exit when GUI_State.Done;
        end loop;

    end Event_Loop;

end DAGBuild.GUI;
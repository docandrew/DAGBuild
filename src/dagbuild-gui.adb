
with SDL.Events.Keyboards;
with SDL.Events.Events;

with SDL.Video.Palettes;
with SDL.Video.Renderers.Makers;
with SDL.Video.Renderers;

with DAGBuild.Settings;

package body DAGBuild.GUI is
   
    -- Draw a SDL Rectangle
    procedure Draw_Rect(r : in out SDL.Video.Renderers.Renderer;
                        x : SDL.Coordinate;
                        y : SDL.Coordinate;
                        w : SDL.Positive_Dimension;
                        h : SDL.Positive_Dimension;
                        c : SDL.Video.Palettes.Colour)
    is
    begin
        r.Set_Draw_Colour (c);
        r.Fill (Rectangle => (x, y, w, h));
    end Draw_Rect;

    -- Clear Screen
    procedure Clear_Window(r : in out SDL.Video.Renderers.Renderer;
                           c : SDL.Video.Palettes.Colour)
    is
    begin
        r.Set_Draw_Colour(c);
        r.Clear;
    end Clear_Window;

    -- Render screen elements
    procedure Render(r : in out SDL.Video.Renderers.Renderer)
    is
    begin
        Clear_Window(r, DAGBuild.Settings.Default_Dark_BG);
        Draw_Rect(r, 64, 48, 64, 48, c => (16#70#, 16#7A#, 16#8C#, 16#FF#));
        r.Present;
        delay 0.100;
    end Render;

    function Handle_Inputs(Event : in out SDL.Events.Events.Events) return Boolean
    is
        Done : Boolean := False;
    begin
        while SDL.Events.Events.Poll(Event) loop

            case Event.Common.Event_Type is
                when SDL.Events.Quit =>
                    Done := True;
            
                when SDL.Events.Keyboards.Key_Down =>
                    
                    case Event.Keyboard.Key_Sym.Key_Code is
                        when SDL.Events.Keyboards.Code_Escape =>
                            Done := True;
                        when others =>
                            null;
                    end case;
                
                when others =>
                    null;
            end case;

        end loop;
        
        return Done;

    end Handle_Inputs;

    procedure Event_Loop(Window : in out SDL.Video.Windows.Window)
    is
        Done : Boolean := False;
        r : SDL.Video.Renderers.Renderer;
        Event : SDL.Events.Events.Events;
    begin
        -- Create hardware renderer if available
        SDL.Video.Renderers.Makers.Create(r, Window);

        loop
            Render(r);

            Done := Handle_Inputs(Event);

            exit when Done;
        end loop;

    end Event_Loop;

end DAGBuild.GUI;
-- with Ada.Numerics.Elementary_Functions;
-- with Ada.Numerics.Generic_Real_Arrays;

-- with Ada.Text_IO; use Ada.Text_IO;

-- with Interfaces.C; use Interfaces.C;

with SDL;
-- with SDL.Timers;
with SDL.Video.Windows;
with SDL.Video.Windows.Makers;
-- with SDL.Video.Renderers;
-- with SDL.Video.Renderers.Makers;
-- with SDL.Video.Palettes;
 with SDL.Events.Events;

with DAGBuild.GUI;
with DAGBuild.Settings;

procedure Main is
    Window      : SDL.Video.Windows.Window;
    -- Renderer    : SDL.Video.Renderers.Renderer;
    -- Quit        : Boolean := False;
    Init_Pos    : constant SDL.Natural_Coordinates := DAGBuild.Settings.Init_Pos;
    Init_Size   : constant SDL.Positive_Sizes := DAGBuild.Settings.Init_Size;

    -- procedure Wait_For_Exit is
    --     use type SDL.Events.Event_Types;
    --     Event : SDL.Events.Events.Events;
    -- begin
    --     loop 
    --         while SDL.Events.Events.Poll(Event) loop
    --             if Event.Common.Event_Type = SDL.Events.Quit then
    --                 return;
    --             end if;
    --         end loop;
    --         delay 0.100;
    --     end loop;
    -- end Wait_For_Exit;
begin

    if not SDL.Initialise then
        raise Program_Error;
    end if;

    SDL.Video.Windows.Makers.Create(Win         => Window, 
                                    Title       => "DAGBuild v0.0.1",
                                    Position    => Init_Pos,
                                    Size        => Init_Size,
                                    Flags       => 0);

    -- Enter main GUI event loop
    DAGBuild.GUI.Event_Loop(Window);
    -- Renderer.Set_Draw_Colour ((0, 0, 0, 255));
    -- Renderer.Clear;
    -- Renderer.Set_Draw_Colour ((255, 0, 0, 255));
    -- Renderer.Draw (Point => (Window.Get_Size.Width / 2, Window.Get_Size.Height / 2));
    -- Renderer.Present;

    -- Wait_For_Exit;
    Window.Finalize;
    SDL.Finalise;
end Main;



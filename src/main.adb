with Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Generic_Real_Arrays;

with Ada.Text_IO; use Ada.Text_IO;

with SDL;
with SDL.Timers;
with SDL.Video.Windows;
with SDL.Video.Windows.Makers;
-- with SDL.Video.Renderers.Makers;
-- with SDL.Video.Palettes;
-- with SDL.Events.Events;

procedure Main is
    Window      : SDL.Video.Windows.Window;
    Init_Pos    : SDL.Natural_Coordinates := 
        (
            SDL.Video.Windows.Undefined_Window_Position(1),
            SDL.Video.Windows.Undefined_Window_Position(1)
        );
    Quit        : Boolean := False;
begin

    if not SDL.Initialise then
        raise Program_Error;
    end if;

    SDL.Video.Windows.Makers.Create(
        Window, "DAGBuild v0.0.1", Init_Pos, (400, 400));

    --Window.Finalize;
    SDL.Finalise;
end Main;



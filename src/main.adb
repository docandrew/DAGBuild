-- with Ada.Numerics.Elementary_Functions;
-- with Ada.Numerics.Generic_Real_Arrays;

-- with Ada.Text_IO; use Ada.Text_IO;

-- with Interfaces.C; use Interfaces.C;

with SDL;

with SDL.Events.Events;

with SDL.Inputs.Keyboards;

with SDL.TTFs.Makers;

with SDL.Video.Windows;
with SDL.Video.Windows.Makers;

with DAGBuild.GUI;
with DAGBuild.Settings;

procedure Main is
    Window      : SDL.Video.Windows.Window;

    Init_Pos    : constant SDL.Natural_Coordinates := DAGBuild.Settings.Init_Pos;
    Init_Size   : constant SDL.Positive_Sizes := DAGBuild.Settings.Init_Size;
begin

    if not SDL.Initialise then
        raise Program_Error with "Unable to load SDL Library";
    end if;

    if not SDL.TTFs.Initialise then
        raise Program_Error with "Unable to load SDL TTF Library";
    end if;

    SDL.Video.Windows.Makers.Create(Win         => Window, 
                                    Title       => "DAGBuild v0.0.1",
                                    Position    => Init_Pos,
                                    Size        => Init_Size,
                                    Flags       => SDL.Video.Windows.Resizable);

    DAGBuild.GUI.Event_Loop(Window);

    Window.Finalize;
    SDL.TTFs.Finalise;
    SDL.Finalise;
end Main;



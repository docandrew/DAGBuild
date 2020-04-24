with SDL.Video.Windows;

package DAGBuild.GUI is

    -- For 60 fps
    Frame_Rate      : constant := 60;
    Milliseconds    : constant Duration := 0.001;
    Delay_Period    : constant Duration := (1 / Frame_Rate) * Milliseconds;

    procedure Event_Loop(Window : in out SDL.Video.Windows.Window);

end DAGBuild.GUI;
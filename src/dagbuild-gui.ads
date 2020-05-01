with Ada.Real_Time;

with SDL.Video.Windows;

package DAGBuild.GUI is

    --@TODO consider making this a setting
    Frame_Rate      : constant := 60;   -- fps
    Delay_Period    : constant Integer := 1000 / Frame_Rate;

    procedure Event_Loop(Window : in out SDL.Video.Windows.Window);

end DAGBuild.GUI;
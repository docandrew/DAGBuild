with Ada.Real_Time;

limited with DAGBuild.GUI.State;

with SDL.TTFs;
with SDL.Video.Palettes;
with SDL.Video.Windows;

package DAGBuild.GUI is

    -- Font used for all widgets.
    DAG_Font        : SDL.TTFs.Fonts;

    --@TODO consider making this a setting
    Frame_Rate      : constant := 60;   -- fps
    Delay_Period    : constant Integer := 1000 / Frame_Rate;

    procedure Start_Render (st : in out DAGBuild.GUI.State.UIState);
    procedure Finish_Render (st : in out DAGBuild.GUI.State.UIState);

    procedure Clear_Window(st : in out DAGBuild.GUI.State.UIState;
                           c : SDL.Video.Palettes.Colour);

    procedure Event_Loop (Window : in out SDL.Video.Windows.Window;
                          Render : access procedure (st : in out DAGBuild.GUI.State.UIState));

end DAGBuild.GUI;
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
    Delay_Period_us : constant Integer := 1_000_000 / Frame_Rate;
    Lock_Frame_Rate : Boolean := True;
    
    -- Length of time required to render a frame.
    Render_Time     : Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds (1);

    procedure Start_Render (st : in out DAGBuild.GUI.State.UIState);
    procedure Finish_Render (st : in out DAGBuild.GUI.State.UIState);

    procedure Clear_Window(st : in out DAGBuild.GUI.State.UIState;
                           c : SDL.Video.Palettes.Colour);

    procedure Event_Loop (Window : in out SDL.Video.Windows.Window;
                          Render : access procedure (st : in out DAGBuild.GUI.State.UIState));

end DAGBuild.GUI;
-- with Ada.Numerics.Elementary_Functions;
-- with Ada.Numerics.Generic_Real_Arrays;
with Ada.Calendar; use Ada.Calendar;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;

-- with Interfaces.C; use Interfaces.C;

with Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding; use Ada.Strings.UTF_Encoding;


with SDL;
with SDL.TTFs;
with SDL.Video.Palettes;
with SDL.Video.Windows;
with SDL.Video.Windows.Makers;

with DAGBuild.GUI;
with DAGBuild.GUI.Settings;
with DAGBuild.GUI.State;
with DAGBuild.GUI.Widgets;
with DAGBuild.GUI.Widgets.Button;       use DAGBuild.GUI.Widgets.Button;
with DAGBuild.GUI.Widgets.Checkbox;     use DAGBuild.GUI.Widgets.Checkbox;
with DAGBuild.GUI.Widgets.Label;        use DAGBuild.GUI.Widgets.Label;
with DAGBuild.GUI.Widgets.Slider;       use DAGBuild.GUI.Widgets.Slider;
with DAGBuild.GUI.Widgets.Spinner;      use DAGBuild.GUI.Widgets.Spinner;
with DAGBuild.GUI.Widgets.Text_Field;   use DAGBuild.GUI.Widgets.Text_Field;
with DAGBuild.GUI.Widgets.Tooltip;      use DAGBuild.GUI.Widgets.Tooltip;

procedure Main is
    Window      : SDL.Video.Windows.Window;

    Init_Pos    : constant SDL.Natural_Coordinates := DAGBuild.GUI.Settings.Init_Pos;
    Init_Size   : constant SDL.Positive_Sizes := DAGBuild.GUI.Settings.Init_Size;

    -- statics for UI demo
    Show_Button : Boolean := False;
    Red         : SDL.Video.Palettes.Colour_Component := 0;
    Green       : SDL.Video.Palettes.Colour_Component := 0;
    Blue        : SDL.Video.Palettes.Colour_Component := 0;
    Some_Int    : Integer := 0;
    Clear_Color : SDL.Video.Palettes.Colour := DAGBuild.GUI.Settings.Default_Dark.Editor_background;
    My_Str      : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String("Input here");
    My_Str2     : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String("Or here");
    
    Smiley_Grin : String := ":)";
    Click       : Boolean := False;
    Dark_Mode   : Boolean := True;
    Show_FPS    : Boolean := False;

    Reload_Default_Background_Color : Boolean := True;

    type Spin_Class is (ONE, TWO, THREE, FOUR, FIVE);
    Spin_Val : Spin_Class := THREE;

    function Spin_Box is new Spinner (Spin_Class);

    -- Render screen elements
    procedure Render(st : in out DAGBuild.GUI.State.UIState)
    is
        use ASCII;
    begin
        if Dark_Mode then
            st.Theme := DAGBuild.GUI.Settings.Default_Dark;
        else
            st.Theme := DAGBuild.GUI.Settings.Default_Light;
        end if;

        if Reload_Default_Background_Color then
            Clear_Color := st.Theme.Editor_Background;
        end if;

        DAGBuild.GUI.Clear_Window(st, Clear_Color);

        Click := Button (st, 50, 50, "One", "This is a tooltip!");

        if Click then
            Clear_Color := (255, 0, 0, 255);
            Show_Button := True;
        end if;

        if Show_Button then
            DAGBuild.GUI.State.Enter_Scope(st);
                Click := Button(st, 50, 250, "Hide Me", "I have a tooltip too.");

                if Click then
                    Clear_Color := st.Theme.Terminal_ANSICyan;
                    Show_Button := False;
                end if;

            DAGBuild.GUI.State.Exit_Scope(st);
        end if;

        Click := Button (st, 150, 50, "Two", " Tooltips!!!");

        if Click then
            Clear_Color := st.Theme.Terminal_ANSIGreen;
        end if;

        Click := Spin_Box (st, 250, 50, Spin_Val);

        Click := Horizontal_Slider (st, 50, 100, 255, Some_Int);

        Click := Button (st, 50, 150, "Three");

        Click := Button (st, 50, 200, "Hello");

        if Click then
            Clear_Color := st.Theme.Terminal_ANSIBlack;
        end if;

        Reload_Default_Background_Color := Checkbox (st, 150, 200, "Dark Mode", Dark_Mode);

        Click := Checkbox (st, 300, 200, "Show frame rate", Show_FPS);

        Click := Checkbox (st, 150, 250, "Lock frame rate at 60 fps", DAGBuild.GUI.Lock_Frame_Rate);

        Click := Button (st, 150, 150, "Quit");
        if Click then
            st.Done := True;
        end if;

        --Slider test
        if Vertical_Slider(st, 500, 40, 255, Integer(Red)) then
            Clear_Color := (Red, Green, Blue, 255);
        end if;

        if Vertical_Slider(st, 550, 40, 255, Integer(Green)) then
            Clear_Color := (Red, Green, Blue, 255);
        end if;

        if Vertical_Slider(st, 600, 40, 255, Integer(Blue)) then
            Clear_Color := (Red, Green, Blue, 255);
        end if;

        if Show_FPS then
            DAGBuild.GUI.State.Enter_Scope (st);
                Label (st, 650, 50, 
                    "Render time   : " & Duration'Image (Ada.Real_Time.To_Duration (DAGBuild.GUI.Render_Time)) & " s");
                Label (st, 650, 75,
                    "Max frame rate: " & Integer'Image (Seconds (1) / DAGBuild.GUI.Render_Time) & " fps");
            DAGBuild.GUI.State.Exit_Scope (st);
        end if;

        Label(st, 50, 300, "Horiz Slider: " & Some_Int'Image, 14);

        Click := Text_Field(st, My_Str, 50, 350, 20, 40);
        Click := Text_Field(st, My_Str2, 50, 400, 20, 40);

        -- If any widgets specified a tooltip, render them now.
        if Ada.Strings.Unbounded.Length (st.Tooltip) /= 0 then
            Tooltip (st);
        end if;
    end Render;
begin
    --@TODO: wrap this up in a DAGBuild.GUI "init"
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

    DAGBuild.GUI.Event_Loop(Window, Render'Access);

    --@TODO: wrap this up in a DAGBuild.GUI "exit" func
    Window.Finalize;
    SDL.TTFs.Finalise;
    SDL.Finalise;
end Main;



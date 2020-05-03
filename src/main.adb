-- with Ada.Numerics.Elementary_Functions;
-- with Ada.Numerics.Generic_Real_Arrays;

-- with Ada.Text_IO; use Ada.Text_IO;

-- with Interfaces.C; use Interfaces.C;

with Ada.Strings.Unbounded;

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
with DAGBuild.GUI.Widgets.Text_Field;   use DAGBuild.GUI.Widgets.Text_Field;

procedure Main is
    Window      : SDL.Video.Windows.Window;

    Init_Pos    : constant SDL.Natural_Coordinates := DAGBuild.GUI.Settings.Init_Pos;
    Init_Size   : constant SDL.Positive_Sizes := DAGBuild.GUI.Settings.Init_Size;

    -- statics for UI demo
    Show_Button : Boolean := False;
    Red         : SDL.Video.Palettes.Colour_Component := 0;
    Green       : SDL.Video.Palettes.Colour_Component := 0;
    Blue        : SDL.Video.Palettes.Colour_Component := 0;
    Clear_Color : SDL.Video.Palettes.Colour := DAGBuild.GUI.Settings.Default_Dark.Editor_background;
    My_Str      : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String("Input here");
    My_Str2     : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String("Or here");
    Click       : Boolean := False;
    Show_Msg    : Boolean := False;

    -- Render screen elements
    procedure Render(st : in out DAGBuild.GUI.State.UIState)
    is
        --package Widgets renames DAGBuild.GUI.Widgets;
    begin
        DAGBuild.GUI.Clear_Window(st, Clear_Color);

        Click := Button (st,
                         50,
                         50,
                         "One");

        if Click then
            Clear_Color := (255, 0, 0, 255);
            Show_Button := True;
        end if;

        if Show_Button then
            DAGBuild.GUI.State.Enter_Scope(st);
                Click := Button(st,
                                250,
                                250,
                                "Hide Me");

                if Click then
                    Clear_Color := st.Theme.InputValidation_errorBackground;
                    Show_Button := False;
                end if;

                --Hidden_Button := (if Click then False else True);
            DAGBuild.GUI.State.Exit_Scope(st);
        end if;

        Click := Button (st,
                         150,
                         50,
                         "Two");

        if Click then
            Clear_Color := st.Theme.InputValidation_infoBackground;
        end if;

        Click := Button (st, 50, 150, "Three");

        Click := Button (st, 50, 200, "Hello");

        if Click then
            Clear_Color := st.Theme.InputValidation_warningBackground;
        end if;

        Checkbox (st, 150, 200, "Show Msg", Show_Msg);

        if Show_Msg then
            DAGBuild.GUI.State.Enter_Scope (st);
            Label (st, "Msg", 50, 250);
            DAGBuild.GUI.State.Exit_Scope (st);
        end if;

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

        Label(st, "Hello DAGBuild!", 50, 300);

        Click := Text_Field(st, My_Str, 50, 350, 20, 40);
        Click := Text_Field(st, My_Str2, 50, 400, 20, 40);
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



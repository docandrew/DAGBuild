with Interfaces.C; use Interfaces.C;

with Ada.Text_IO;

with SDL;
with SDL.Video.Palettes;
with SDL.Video.Renderers;

with DAGBuild.Settings;

package body DAGBuild.GUI.Widgets is

    -- See if mouse position is within a bounded box
    function Region_Hit(st  : DAGBuild.GUI.State.UIState;
                        x   : SDL.Natural_Coordinate;
                        y   : SDL.Natural_Coordinate;
                        w   : SDL.Positive_Dimension;
                        h   : SDL.Positive_Dimension) return Boolean
    is
    begin
        if  st.Mouse_x < x or
            st.Mouse_y < y or
            st.Mouse_x >= x + w or
            st.Mouse_y >= y + h then
            return False;
        end if;

        return True;
    end Region_Hit;

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

    function Button(st  : in out DAGBuild.GUI.State.UIState;
                    x   : SDL.Natural_Coordinate;
                    y   : SDL.Natural_Coordinate) return Boolean
    is
        use DAGBuild.GUI.State;

        id      : constant DAGBuild.GUI.State.ID := DAGBuild.GUI.State.Next_ID(st);
        Scope   : constant DAGBuild.GUI.State.Scope := st.Curr_Scope;

    begin
        --Ada.Text_IO.Put_Line("Creating new button with ID: " & id'Image & " scope: " & scope'Image);

        if Region_Hit(st, x, y, 64, 48) then
            st.Hot_Item := id;
            st.Hot_Scope := Scope;

            if st.Active_Item = NO_ITEM and st.Mouse_Down then
                st.Active_Item := id;
                st.Active_Scope := Scope;
            end if;
        end if;

        --Ada.Text_IO.Put_Line("Active: " & st.Active_Item'Image & " Hot: " & st.Hot_Item'Image);
        
        -- Render a button shadow
        Draw_Rect(st.Renderer,
                  x+2,
                  y+2,
                  64,
                  48,
                  (0,0,0,0));
        
        if st.Hot_Item = id and st.Hot_Scope = Scope then
            if st.Active_Item = id and st.Active_Scope = Scope then
                -- Hot and Active button - depress
                Draw_Rect(st.Renderer,
                          x+2,
                          y+2,
                          64,
                          48,
                          DAGBuild.Settings.Dark_Active);
            else
                -- Hot but not Active - 
                Draw_Rect(st.Renderer,
                          x,
                          y,
                          64,
                          48,
                          DAGBuild.Settings.Dark_Hot);
            end if;
        else
            -- Not hot, could be active
            Draw_Rect(st.Renderer,
                      x,
                      y,
                      64,
                      48,
                      DAGBuild.Settings.Dark_Widget);
        end if;

        -- If button is hot and active, but button not up, then it was clicked
        if st.Mouse_Down = False and 
            st.Hot_Item = id and st.Hot_Scope = Scope and
            st.Active_Item = id and st.Active_Scope = Scope then
           return True;
        end if;

        return False;
    end Button;

    function Slider(st  : in out DAGBuild.GUI.State.UIState;
                    x   : SDL.Natural_Coordinate;
                    y   : SDL.Natural_Coordinate;
                    Max : Integer;
                    Val : in out Integer) return Boolean
    is
        use DAGBuild.GUI.State;

        id      : constant DAGBuild.GUI.State.ID := DAGBuild.GUI.State.Next_ID(st);
        scope   : constant DAGBuild.GUI.State.Scope := st.Curr_Scope;

        ypos    : constant SDL.Natural_Coordinate := SDL.Natural_Coordinate(((256 - 16) * Val) / Max);
        
    begin

        if Region_Hit(st, x + 8, y + 8, 16, 255) then
            st.Hot_Item := id;
            st.Hot_Scope := scope;

            if st.Active_Item = NO_ITEM and st.Mouse_Down then
                st.Active_Item := id;
                st.Active_Scope := scope;
            end if;
        end if;

        -- Draw the scrollbar
        Draw_Rect(st.Renderer, x, y, 32, 256+16, DAGBuild.Settings.Dark_Widget);

        -- Highlight the slider
        if (st.Hot_Item = id and st.Hot_Scope = Scope) or 
            (st.Active_Item = id and st.Active_Scope = Scope) then
            Draw_Rect ( st.Renderer,
                        x + 8,
                        y + 8 + ypos,
                        16,
                        16,
                        DAGBuild.Settings.Dark_Active);
        else
            Draw_Rect ( st.Renderer,
                        x + 8, 
                        y + 8 + ypos,
                        16,
                        16,
                        DAGBuild.Settings.Dark_Hot);
        end if;

        -- Update slider position
        if st.Active_Item = id and st.Active_Scope = Scope then
            Update: declare
                Mouse_Pos : SDL.Natural_Coordinate := st.Mouse_y - (y + 8);
                New_Val : Integer;
            begin

                -- if Mouse_Pos < 0 then
                --     Mouse_Pos := 0;
                -- end if;

                if Mouse_Pos > 255 then
                    Mouse_Pos := 255;
                end if;

                New_Val := (Integer(Mouse_Pos) * Max) / 255;

                if Val /= New_Val then
                    Val := New_Val;
                    return True;
                end if;
            end Update;
        end if;

        return False;
    end Slider;

end DAGBuild.GUI.Widgets;

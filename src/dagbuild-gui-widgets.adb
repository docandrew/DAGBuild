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

    -- Draw a button, return True if clicked
    function Button(st  : in out DAGBuild.GUI.State.UIState;
                    x   : SDL.Natural_Coordinate;
                    y   : SDL.Natural_Coordinate) return Boolean
    is
        use DAGBuild.GUI.State;
        id      : DAGBuild.GUI.State.ID := DAGBuild.GUI.State.Next_ID(st);
        scope   : DAGBuild.GUI.State.Scope := st.Curr_Scope;
    begin
        --Ada.Text_IO.Put_Line("Creating new button with ID: " & id'Image & " scope: " & scope'Image);

        if Region_Hit(st, x, y, 64, 48) then
            st.Hot_Item := id;
            st.Hot_Scope := scope;

            if st.Active_Item = NO_ITEM and st.Mouse_Down then
                st.Active_Item := id;
                st.Active_Scope := scope;
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
        
        if st.Hot_Item = id and st.Hot_Scope = scope then
            if st.Active_Item = id and st.Active_Scope = scope then
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
                          DAGBuild.Settings.Dark_Active);
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
           st.Hot_Item = id and st.Hot_Scope = scope and
           st.Active_Item = id and st.Active_Scope = scope then
           return True;
        end if;

        return False;
    end Button;

end DAGBuild.GUI.Widgets;

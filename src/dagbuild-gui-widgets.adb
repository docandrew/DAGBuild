with Interfaces.C; use Interfaces.C;

--with Ada.Text_IO;

with SDL;

with SDL.Events;
with SDL.Events.Keyboards;

with SDL.Video.Palettes;
with SDL.Video.Rectangles;
with SDL.Video.Renderers;
with SDL.Video.Surfaces;
with SDL.Video.Textures;
with SDL.Video.Textures.Makers;

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

    -- Draw a filled SDL Rectangle
    procedure Draw_Rect (r      : in out SDL.Video.Renderers.Renderer;
                         x      : SDL.Coordinate;
                         y      : SDL.Coordinate;
                         w      : SDL.Positive_Dimension;
                         h      : SDL.Positive_Dimension;
                         Color  : SDL.Video.Palettes.Colour)
    is
    begin
        r.Set_Draw_Colour (Color);
        r.Fill (Rectangle => (x, y, w, h));
    end Draw_Rect;

    -- Draw an outlined SDL Rectangle
    procedure Outline_Rect (r       : in out SDL.Video.Renderers.Renderer;
                            x       : SDL.Coordinate;
                            y       : SDL.Coordinate;
                            w       : SDL.Positive_Dimension;
                            h       : SDL.Positive_Dimension;
                            Color   : SDL.Video.Palettes.Colour)
    is
    begin
        r.Set_Draw_Colour (Color);
        r.Draw (Rectangle => (x, y, w, h));
    end Outline_Rect;

    -- Draw text at a specified location 
    -- @field x is top-left corner
    -- @field y is top-left corner
    -- @field w is the width of the drawn text (output)
    -- @field h is the height of the drawn text (output)
    -- @field Colour is the color of the text to draw
    procedure Draw_Text (r      : in out SDL.Video.Renderers.Renderer;
                         Text   : String;
                         x      : SDL.Coordinate;
                         y      : SDL.Coordinate;
                         w      : out SDL.Positive_Dimension;
                         h      : out SDL.Positive_Dimension;
                         Color  : SDL.Video.Palettes.Colour)
    is
        Text_Surface    : SDL.Video.Surfaces.Surface;
        Text_Texture    : SDL.Video.Textures.Texture;
        Text_Rect       : SDL.Video.Rectangles.Rectangle;
    begin

        if Text'Length = 0 then
            w := 1;
            h := 1;
            return;
        end if;

        Text_Surface := DAG_Font.Render_UTF_8_Blended (Text    => Text,
                                                      Colour  => Color);

        SDL.Video.Textures.Makers.Create (Tex       => Text_Texture, 
                                          Renderer  => r,
                                          Surface   => Text_Surface);

        Text_Rect.X := x;
        Text_Rect.Y := y;
        Text_Rect.Width := Text_Texture.Get_Size.Width;
        Text_Rect.Height := Text_Texture.Get_Size.Height;
        w := Text_Rect.Width;
        h := Text_Rect.Height;

        -- Blit text from the texture into the renderer
        r.Copy (Copy_From   => Text_Texture,
                To          => Text_Rect);

        --Text_Surface.Finalize;
        --Text_Texture.Finalize;
    end Draw_Text;

    -- Draw a button, return True if clicked, False otherwise
    function Button (st     : in out DAGBuild.GUI.State.UIState;
                     x      : SDL.Natural_Coordinate;
                     y      : SDL.Natural_Coordinate;
                     Label  : String := "") return Boolean
    is
        use DAGBuild.GUI.State;

        id      : constant DAGBuild.GUI.State.ID := DAGBuild.GUI.State.Next_ID(st);
        Scope   : constant DAGBuild.GUI.State.Scope := st.Curr_Scope;
        Dummy_w : SDL.Positive_Dimension;
        Dummy_h : SDL.Positive_Dimension;
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
        -- Keyboard Focus Handling

        -- if no widget has keyboard focus, take it
        if st.Kbd_Item = NO_ITEM then
            st.Kbd_Item := id;
            st.Kbd_Scope := Scope;
        end if;

        -- if we have keyboard focus, show it and update heartbeat
        if st.Kbd_Item = id and st.Kbd_Scope = Scope then
            Outline_Rect(st.Renderer,
                        x-2,
                        y-2,
                        70,
                        54,
                        DAGBuild.Settings.Dark_Focus);

            st.Kbd_Heartbeat := True;
        end if;
        
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

        -- Draw Label
        Draw_Text (r => st.Renderer,
                   Text => Label,
                   x => x+4,
                   y => y+8,
                   w => Dummy_w,
                   h => Dummy_h,
                   Color => DAGBuild.Settings.Dark_Text);

        -- Keyboard input processing
        HandleKeys: declare
            use SDL.Events.Keyboards;
        begin
            if st.Kbd_Item = id and st.Kbd_Scope = Scope then
                case st.Kbd_Pressed is
                    when SDL.Events.Keyboards.Code_Tab =>
                        -- Lose focus, next widget will snag it.
                        st.Kbd_Item := NO_ITEM;
                        st.Kbd_Scope := NO_SCOPE;

                        -- or make previous widget get focus if we're doing shift+tab.
                        if (st.Kbd_Modifier and Modifier_Shift) /= 0 then
                            st.Kbd_Item := st.Last_Widget;
                            st.Kbd_Scope := st.Last_Scope;
                        end if;

                        -- clear key
                        st.Kbd_Pressed := NO_KEY;

                    when SDL.Events.Keyboards.Code_Return =>
                        -- act like a press occurred.
                        st.Kbd_Pressed := NO_KEY;
                        return True;
                    when others =>
                        null;
                end case;
            end if;
        end HandleKeys;

        st.Last_Widget := id;
        st.Last_Scope := Scope;

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

        -- if no widget has keyboard focus, take it
        if st.Kbd_Item = NO_ITEM then
            st.Kbd_Item := id;
            st.Kbd_Scope := Scope;
        end if;

        -- if we have keyboard focus, show it and update heartbeat
        if st.Kbd_Item = id and st.Kbd_Scope = Scope then
            Outline_Rect(st.Renderer,
                        x-2,
                        y-2,
                        36,
                        256+16+4,
                        (16#5C#,16#CF#,16#E6#,255));

            st.Kbd_Heartbeat := True;
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
                Mouse_Pos : Integer := Integer(st.Mouse_y) - 
                                        Integer(y + 8);
                New_Val : Integer;
            begin

                if Mouse_Pos < 0 then
                    Mouse_Pos := 0;
                end if;

                if Mouse_Pos > 255 then
                    Mouse_Pos := 255;
                end if;

                New_Val := (Mouse_Pos * Max) / 255;

                if Val /= New_Val then
                    Val := New_Val;
                    return True;
                end if;
            end Update;
        end if;

        -- If we have keyboard focus, process keys
        HandleKeys: Declare
            use SDL.Events.Keyboards;
        begin    
            if st.Kbd_Item = id and st.Kbd_Scope = Scope then
                --Ada.Text_IO.Put_Line("slider key: " & st.Kbd_Pressed'Image);

                case st.Kbd_Pressed is
                    when SDL.Events.Keyboards.Code_Tab =>
                        -- Lose focus, next widget will snag it.
                        st.Kbd_Item := NO_ITEM;
                        st.Kbd_Scope := NO_SCOPE;

                        -- or make previous widget get focus if we're doing shift+tab.
                        if (st.Kbd_Modifier and Modifier_Shift) /= 0 then
                            st.Kbd_Item := st.Last_Widget;
                            st.Kbd_Scope := st.Last_Scope;
                        end if;

                        -- clear key
                        st.Kbd_Pressed := NO_KEY;

                    when SDL.Events.Keyboards.Code_Up =>
                        if Val > 0 then
                            Val := Val - 1;
                        end if;

                        st.Kbd_Pressed := NO_KEY;
                        return True;

                    when SDL.Events.Keyboards.Code_Down =>
                        if Val < Max then
                            Val := Val + 1;
                        end if;

                        st.Kbd_Pressed := NO_KEY;
                        return True;

                    when others =>
                        --Ada.Text_IO.Put_Line("other: " & st.Kbd_Pressed'Image);
                        null;
                end case;
            end if;
        end HandleKeys;

        st.Last_Widget := id;
        st.Last_Scope := Scope;

        return False;
    end Slider;

    -- Draw a label with the given text a specific location
    procedure Label(st      : in out DAGBuild.GUI.State.UIState;
                    Text    : String;
                    x       : SDL.Natural_Coordinate;
                    y       : SDL.Natural_Coordinate)
    is
        w : SDL.Dimension;
        h : SDL.Dimension;
    begin
        Draw_Rect (st.Renderer,
                   x,
                   y,
                   140,
                   32,
                   DAGBuild.Settings.Dark_Widget);

        Draw_Text (r => st.Renderer,
                   Text => Text, 
                   x => x+4,
                   y => y+8,
                   w => w,
                   h => h,
                   Color => DAGBuild.Settings.Dark_Text);
    end Label;

    -- Draw a label with the given text a specific location
    -- @TODO: make Display_Length, Max_Length work with a number of chars
    function Text_Field (st              : in out DAGBuild.GUI.State.UIState;
                         Text            : in out Ada.Strings.Unbounded.Unbounded_String;
                         x               : SDL.Natural_Coordinate;
                         y               : SDL.Natural_Coordinate;
                         Display_Length  : SDL.Positive_Dimension;
                         Max_Length      : Natural) return Boolean
    is
        package UBS renames Ada.Strings.Unbounded;

        w : SDL.Dimension;
        h : SDL.Dimension;
    begin
        Draw_Rect (st.Renderer,
                   x,
                   y,
                   140,
                   32,
                   DAGBuild.Settings.Dark_Widget);

        -- Clip the number of characters by what we can actually display
        Draw_Text (r => st.Renderer,
                   Text => UBS.To_String(UBS.Unbounded_Slice(Text, 1, UBS.Length(Text))),
                   x => x+4,
                   y => y+8,
                   w => w,
                   h => h,
                   Color => DAGBuild.Settings.Dark_Text);

        return True;
    end Text_Field;

end DAGBuild.GUI.Widgets;

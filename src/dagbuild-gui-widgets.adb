--with Ada.Real_Time;
with Ada.Text_IO;

with Interfaces.C; use Interfaces.C;

with SDL;
with SDL.Events;
-- with SDL.Events.Keyboards;
-- with SDL.Video.Palettes;
with SDL.Video.Rectangles;
-- with SDL.Video.Renderers;
with SDL.Video.Surfaces;
with SDL.Video.Textures;
with SDL.Video.Textures.Makers;

-- with DAGBuild.GUI.Settings;
--@TODO change r to st to use scope x,y offset.
package body DAGBuild.GUI.Widgets is

    -- See if mouse position is within a bounded box
    function Region_Hit(st  : DAGBuild.GUI.State.UIState;
                        x   : SDL.Natural_Coordinate;
                        y   : SDL.Natural_Coordinate;
                        w   : SDL.Positive_Dimension;
                        h   : SDL.Positive_Dimension) return Boolean
    is
        Adjusted_x : constant SDL.Natural_Coordinate := x + st.Scope_X_Offset;
        Adjusted_y : constant SDL.Natural_Coordinate := y + st.Scope_Y_Offset;
    begin
        if st.Mouse_x < Adjusted_x or
           st.Mouse_y < Adjusted_y or
           st.Mouse_x >= Adjusted_x + w or
           st.Mouse_y >= Adjusted_y + h then
           return False;
        end if;

        return True;
    end Region_Hit;

    -- Draw a single line
    procedure Draw_Line (st     : in out DAGBuild.GUI.State.UIState;
                         x1     : SDL.Coordinate;
                         y1     : SDL.Coordinate;
                         x2     : SDL.Coordinate;
                         y2     : SDL.Coordinate;
                         Color  : SDL.Video.Palettes.Colour)
    is
        Adjusted_x1 : constant SDL.Natural_Coordinate := x1 + st.Scope_X_Offset;
        Adjusted_x2 : constant SDL.Natural_Coordinate := x2 + st.Scope_X_Offset;
        Adjusted_y1 : constant SDL.Natural_Coordinate := y1 + st.Scope_Y_Offset;
        Adjusted_y2 : constant SDL.Natural_Coordinate := y2 + st.Scope_Y_Offset;
    begin
        st.Renderer.Set_Draw_Colour (Color);
        st.Renderer.Draw (Line => ((Adjusted_x1, Adjusted_y1), (Adjusted_x2, Adjusted_y2)));
    end Draw_Line;

    -- Draw a filled SDL Rectangle
    procedure Draw_Rect (st     : in out DAGBuild.GUI.State.UIState;
                         x      : SDL.Coordinate;
                         y      : SDL.Coordinate;
                         w      : SDL.Positive_Dimension;
                         h      : SDL.Positive_Dimension;
                         Color  : SDL.Video.Palettes.Colour)
    is
        Adjusted_x : constant SDL.Natural_Coordinate := x + st.Scope_X_Offset;
        Adjusted_y : constant SDL.Natural_Coordinate := y + st.Scope_Y_Offset;
    begin
        st.Renderer.Set_Draw_Colour (Color);
        st.Renderer.Fill (Rectangle => (Adjusted_x, Adjusted_y, w, h));
    end Draw_Rect;

    -- Draw an outlined SDL Rectangle
    procedure Outline_Rect (st      : in out DAGBuild.GUI.State.UIState;
                            x       : SDL.Coordinate;
                            y       : SDL.Coordinate;
                            w       : SDL.Positive_Dimension;
                            h       : SDL.Positive_Dimension;
                            Color   : SDL.Video.Palettes.Colour)
    is
        Adjusted_x : constant SDL.Natural_Coordinate := x + st.Scope_X_Offset;
        Adjusted_y : constant SDL.Natural_Coordinate := y + st.Scope_Y_Offset;
    begin
        st.Renderer.Set_Draw_Colour (Color);
        st.Renderer.Draw (Rectangle => (Adjusted_x, Adjusted_y, w, h));
    end Outline_Rect;


    procedure Draw_Text (st         : in out DAGBuild.GUI.State.UIState;
                         Text       : String;
                         x          : SDL.Coordinate;
                         y          : SDL.Coordinate;
                         w          : out SDL.Dimension;
                         h          : out SDL.Dimension;
                         Color      : SDL.Video.Palettes.Colour;
                         BG_Color   : SDL.Video.Palettes.Colour;
                         Padding    : SDL.Natural_Dimension := 0)
    is
        Adjusted_x      : constant SDL.Natural_Coordinate := x + st.Scope_X_Offset;
        Adjusted_y      : constant SDL.Natural_Coordinate := y + st.Scope_Y_Offset;
        Text_Surface    : SDL.Video.Surfaces.Surface;
        Text_Texture    : SDL.Video.Textures.Texture;
        Text_Rect       : SDL.Video.Rectangles.Rectangle;
    begin

        if Text'Length = 0 then
            w := 0;
            h := 0;
            return;
        end if;

        Text_Surface := DAG_Font.Render_UTF_8_Blended (Text    => Text,
                                                       Colour  => Color);
        -- Text_Surface := DAG_Font.Render_UTF_8_Shaded (Text              => Text,
        --                                               Colour            => Color,
        --                                               Background_Colour => BG_Color);

                                                      

        SDL.Video.Textures.Makers.Create (Tex       => Text_Texture,
                                          Renderer  => st.Renderer,
                                          Surface   => Text_Surface);

        Text_Rect.X         := Adjusted_x + Padding;
        Text_Rect.Y         := Adjusted_y + Padding;
        Text_Rect.Width     := Text_Texture.Get_Size.Width;
        Text_Rect.Height    := Text_Texture.Get_Size.Height;
        w                   := Text_Rect.Width + 2 * Padding;
        h                   := Text_Rect.Height + 2 * Padding;

        -- Draw an underlying rectangle of the BG color.
        Draw_Rect (st, Adjusted_x, Adjusted_y, w, h, BG_Color);

        -- Blit text from the texture into the renderer
        st.Renderer.Copy (Copy_From   => Text_Texture,
                          To          => Text_Rect);

        --Text_Surface.Finalize;
        --Text_Texture.Finalize;
    end Draw_Text;

end DAGBuild.GUI.Widgets;

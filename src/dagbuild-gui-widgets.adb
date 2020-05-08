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

    -- Draw a single line
    procedure Draw_Line (r      : in out SDL.Video.Renderers.Renderer;
                         x1     : SDL.Coordinate;
                         y1     : SDL.Coordinate;
                         x2     : SDL.Coordinate;
                         y2     : SDL.Coordinate;
                         Color  : SDL.Video.Palettes.Colour)
    is
    begin
        r.Set_Draw_Colour (Color);
        r.Draw (Line => ((x1, y1), (x2, y2)));
    end Draw_Line;

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


    procedure Draw_Text (r          : in out SDL.Video.Renderers.Renderer;
                         Text       : String;
                         x          : SDL.Coordinate;
                         y          : SDL.Coordinate;
                         w          : out SDL.Dimension;
                         h          : out SDL.Dimension;
                         Color      : SDL.Video.Palettes.Colour;
                         BG_Color   : SDL.Video.Palettes.Colour;
                         Padding    : SDL.Natural_Dimension := 0)
    is
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
                                          Renderer  => r,
                                          Surface   => Text_Surface);

        Text_Rect.X := x + Padding;
        Text_Rect.Y := y + Padding;
        Text_Rect.Width := Text_Texture.Get_Size.Width;
        Text_Rect.Height := Text_Texture.Get_Size.Height;
        w := Text_Rect.Width + 2 * Padding;
        h := Text_Rect.Height + 2 * Padding;

        -- Draw an underlying rectangle of the BG color.
        Draw_Rect (r, x, y, w, h, BG_Color);

        -- Blit text from the texture into the renderer
        r.Copy (Copy_From   => Text_Texture,
                To          => Text_Rect);

        --Text_Surface.Finalize;
        --Text_Texture.Finalize;
    end Draw_Text;

end DAGBuild.GUI.Widgets;

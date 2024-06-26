-- with Ada.Strings.Unbounded;

with DAGBuild.GUI.State;

--with SDL.TTFs;
with SDL.Video.Palettes;
with SDL.Video.Renderers;

package DAGBuild.GUI.Widgets is

-- Interface primitives used by child widget packages
private

    -- See if mouse position is within a bounded box
    function Region_Hit (st  : DAGBuild.GUI.State.UIState;
                         x   : SDL.Natural_Coordinate;
                         y   : SDL.Natural_Coordinate;
                         w   : SDL.Positive_Dimension;
                         h   : SDL.Positive_Dimension) return Boolean;

    -- Render a single line
    procedure Draw_Line (st     : in out DAGBuild.GUI.State.UIState;
                         x1     : SDL.Coordinate;
                         y1     : SDL.Coordinate;
                         x2     : SDL.Coordinate;
                         y2     : SDL.Coordinate;
                         Color  : SDL.Video.Palettes.Colour);

    -- Render a filled SDL Rectangle
    procedure Draw_Rect (st     : in out DAGBuild.GUI.State.UIState;
                         x      : SDL.Coordinate;
                         y      : SDL.Coordinate;
                         w      : SDL.Positive_Dimension;
                         h      : SDL.Positive_Dimension;
                         Color  : SDL.Video.Palettes.Colour);

    -- Render an outlined SDL Rectangle
    procedure Outline_Rect (st      : in out DAGBuild.GUI.State.UIState;
                            x       : SDL.Coordinate;
                            y       : SDL.Coordinate;
                            w       : SDL.Positive_Dimension;
                            h       : SDL.Positive_Dimension;
                            Color   : SDL.Video.Palettes.Colour);

    -- Render text at a specified location 
    -- @field x is top-left corner
    -- @field y is top-left corner
    -- @field w is the width of the drawn text plus the padding on either
    --  side of the text, if specified. (output)
    -- @field h is the height of the drawn text plus the padding on top and
    --  bottom of the text, if specified. (output)
    -- @field Color is the color of the text to draw
    -- @field BG_Color is the background color to use
    -- @field Padding surrounds the text with pixels of the background color
    --  on all sides.
    procedure Draw_Text (st         : in out DAGBuild.GUI.State.UIState;
                         Text       : String;
                         x          : SDL.Coordinate;
                         y          : SDL.Coordinate;
                         w          : out SDL.Dimension;
                         h          : out SDL.Dimension;
                         Color      : SDL.Video.Palettes.Colour;
                         BG_Color   : SDL.Video.Palettes.Colour;
                         Padding    : SDL.Natural_Dimension := 0);

end DAGBuild.GUI.Widgets;

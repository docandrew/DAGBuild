with Ada.Real_Time;
with Ada.Text_IO;

with Interfaces.C; use Interfaces.C;

with SDL;
with SDL.Events;
with SDL.Events.Keyboards;
with SDL.Inputs.Mice.Cursors;
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

    -- Draw text at a specified location 
    -- @field x is top-left corner
    -- @field y is top-left corner
    -- @field w is the width of the drawn text (output)
    -- @field h is the height of the drawn text (output)
    -- @field Color is the color of the text to draw
    -- @field BG_Color is the background color to use
    procedure Draw_Text (r          : in out SDL.Video.Renderers.Renderer;
                         Text       : String;
                         x          : SDL.Coordinate;
                         y          : SDL.Coordinate;
                         w          : out SDL.Dimension;
                         h          : out SDL.Dimension;
                         Color      : SDL.Video.Palettes.Colour;
                         BG_Color   : SDL.Video.Palettes.Colour)
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

        -- Text_Surface := DAG_Font.Render_UTF_8_Blended (Text    => Text,
        --                                                Colour  => Color);
        Text_Surface := DAG_Font.Render_UTF_8_Shaded (Text              => Text,
                                                      Colour            => Color,
                                                      Background_Colour => BG_Color);

                                                      

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

    -- Draw a button, return True if clicked, False otherwise.
    function Button (st             : in out DAGBuild.GUI.State.UIState;
                     x              : SDL.Natural_Coordinate;
                     y              : SDL.Natural_Coordinate;
                     Label          : String := "") return Boolean
    is
        Button_Width     : constant SDL.Positive_Dimension := SDL.Positive_Dimension(DAGBuild.Settings.Font_Size) * 
                                                                 SDL.Positive_Dimension(5);
        Button_Height    : constant SDL.Positive_Dimension := 2 * SDL.Positive_Dimension(DAGBuild.Settings.Font_Size);
    begin
        return Button(st, x, y, Label, Button_Width, Button_Height);
    end Button;

    -- Draw a button, return True if clicked, False otherwise
    function Button (st             : in out DAGBuild.GUI.State.UIState;
                     x              : SDL.Natural_Coordinate;
                     y              : SDL.Natural_Coordinate;
                     Label          : String := "";
                     Button_Width   : SDL.Natural_Coordinate;
                     Button_Height  : SDL.Natural_Coordinate) return Boolean
    is
        use DAGBuild.GUI.State;

        id      : constant DAGBuild.GUI.State.ID := DAGBuild.GUI.State.Next_ID(st);
        Scope   : constant DAGBuild.GUI.State.Scope := st.Curr_Scope;
        
        Dummy_w : SDL.Positive_Dimension;
        Dummy_h : SDL.Positive_Dimension;
        
        Button_Shadow_Offset : constant SDL.Positive_Dimension := 2;
        
        -- Button_Width    : constant SDL.Positive_Dimension := 64;
        -- Button_Height   : constant SDL.Positive_Dimension := 40;

        --@TODO make this a function of font size
        Label_x : constant SDL.Positive_Dimension := x + 4;
        Label_y : constant SDL.Positive_Dimension := y + 4;
    begin
        --Ada.Text_IO.Put_Line("Creating new button with ID: " & id'Image & " scope: " & scope'Image);

        if Region_Hit (st, x, y, Button_Width, Button_Height) then
            st.Hot_Item     := id;
            st.Hot_Scope    := Scope;

            if st.Active_Item = NO_ITEM and st.Mouse_Down then
                st.Active_Item  := id;
                st.Active_Scope := Scope;
            end if;
        end if;

        --Ada.Text_IO.Put_Line("Active: " & st.Active_Item'Image & " Hot: " & st.Hot_Item'Image);
        -- Keyboard Focus Handling

        -- if no widget has keyboard focus, take it
        if st.Kbd_Item = NO_ITEM then
            st.Kbd_Item     := id;
            st.Kbd_Scope    := Scope;
        end if;

        -- if we have keyboard focus, show it and update heartbeat
        if st.Kbd_Item = id and st.Kbd_Scope = Scope then
            Outline_Rect (st.Renderer,
                          x - 2,
                          y - 2,
                          Button_Width + 6,
                          Button_Height + 6,
                          st.Theme.InputOption_activeBorder);

            st.Kbd_Heartbeat := True;
        end if;
        
        -- Render a button shadow
        Draw_Rect (st.Renderer,
                   x + Button_Shadow_Offset,
                   y + Button_Shadow_Offset,
                   Button_Width,
                   Button_Height,
                   st.Theme.Widget_Shadow);
        
        if st.Hot_Item = id and st.Hot_Scope = Scope then
            if st.Active_Item = id and st.Active_Scope = Scope then
                -- Hot and Active button - depress
                Draw_Rect (st.Renderer,
                           x + 2,
                           y + 2,
                           Button_Width,
                           Button_Height,
                           st.Theme.Button_HoverBackground);

                -- Draw Label
                Draw_Text (r        => st.Renderer,
                           Text     => Label,
                           x        => Label_x + 2,
                           y        => Label_y + 2,
                           w        => Dummy_w,
                           h        => Dummy_h,
                           Color    => st.Theme.Button_Foreground,
                           BG_Color => st.Theme.Button_HoverBackground);
                --@TODO play audio of label for accessibility
            else
                -- Hot but not Active - 
                Draw_Rect(st.Renderer,
                          x,
                          y,
                          Button_Width,
                          Button_Height,
                          st.Theme.Button_HoverBackground);

                Draw_Text (r        => st.Renderer,
                           Text     => Label,
                           x        => Label_x,
                           y        => Label_y,
                           w        => Dummy_w,
                           h        => Dummy_h,
                           Color    => st.Theme.Button_Foreground,
                           BG_Color => st.Theme.Button_HoverBackground);
                --@TODO play audio of label for accessibility
            end if;
        else
            -- Not hot, could be active
            Draw_Rect (st.Renderer,
                       x,
                       y,
                       Button_Width,
                       Button_Height,
                       st.Theme.Button_Background);

            Draw_Text (r        => st.Renderer,
                       Text     => Label,
                       x        => Label_x,
                       y        => Label_y,
                       w        => Dummy_w,
                       h        => Dummy_h,
                       Color    => st.Theme.Button_Foreground,
                       BG_Color => st.Theme.Button_Background);
        end if;

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

    function Vertical_Slider(st  : in out DAGBuild.GUI.State.UIState;
                             x   : SDL.Natural_Coordinate;
                             y   : SDL.Natural_Coordinate;
                             Max : Integer;
                             Val : in out Integer) return Boolean
    is
        use DAGBuild.GUI.State;

        id      : constant DAGBuild.GUI.State.ID := DAGBuild.GUI.State.Next_ID(st);
        scope   : constant DAGBuild.GUI.State.Scope := st.Curr_Scope;

        Slider_Width    : constant SDL.Positive_Dimension := 32;
        Slider_Height   : constant SDL.Positive_Dimension := 255;
        
        Button_Width    : constant SDL.Positive_Dimension := 16;
        Button_Height   : constant SDL.Positive_Dimension := 16;
        
        ypos : constant SDL.Natural_Coordinate :=
            SDL.Natural_Coordinate((Integer(Slider_Height - 16) * Val) / Max);
    begin

        if Region_Hit(st, x + 8, y + 8, 16, Slider_Height) then
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
                        x - 2,
                        y - 2,
                        Slider_Width + 4,
                        Slider_Height + 16 + 4,
                        st.Theme.InputOption_activeBorder);

            st.Kbd_Heartbeat := True;
        end if;

        -- Draw the scrollbar
        Draw_Rect (st.Renderer,
                   x,
                   y,
                   Slider_Width,
                   Slider_Height + 16,
                   st.Theme.Scrollbar_shadow);

        -- Highlight the button
        if (st.Hot_Item = id and st.Hot_Scope = Scope) or 
            (st.Active_Item = id and st.Active_Scope = Scope) then
            Draw_Rect ( st.Renderer,
                        x + 8,
                        y + 8 + ypos,
                        Button_Width,
                        Button_Height,
                        st.Theme.ScrollbarSlider_activeBackground);
        else
            Draw_Rect ( st.Renderer,
                        x + 8, 
                        y + 8 + ypos,
                        Button_Width,
                        Button_Height,
                        st.Theme.ScrollbarSlider_background);
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
    end Vertical_Slider;

    -- Draw a label with the given text a specific location
    procedure Label(st              : in out DAGBuild.GUI.State.UIState;
                    Text            : String;
                    x               : SDL.Natural_Coordinate;
                    y               : SDL.Natural_Coordinate;
                    Display_Length  : Natural := 10)
    is
        -- Labels have an id and scope, but we ignore them
        id      : constant DAGBuild.GUI.State.ID := DAGBuild.GUI.State.Next_ID(st);
        scope   : constant DAGBuild.GUI.State.Scope := st.Curr_Scope;
        pragma Unreferenced(id, scope);

        Field_Width     : constant SDL.Positive_Dimension := SDL.Positive_Dimension(DAGBuild.Settings.Font_Size) * 
                                                                 SDL.Positive_Dimension(Display_Length);
        Field_Height    : constant SDL.Positive_Dimension := 2 * SDL.Positive_Dimension(DAGBuild.Settings.Font_Size);
        
        -- How far inside the field to start drawing characters
        Text_Draw_Offset : constant SDL.Positive_Dimension := 4;

        w : SDL.Dimension;
        h : SDL.Dimension;
    begin
        Draw_Rect (st.Renderer,
                   x,
                   y,
                   Field_Width,
                   Field_Height,
                   st.Theme.Input_Background);

        Draw_Text (r        => st.Renderer,
                   Text     => Text, 
                   x        => x + Text_Draw_Offset,
                   y        => y + 8,
                   w        => w,
                   h        => h,
                   Color    => st.Theme.Input_Foreground,
                   BG_Color => st.Theme.Input_Background);
    end Label;

    -- For ctrl+click and double-click whole-word expansion
    function Is_Word_End (Char : Character) return Boolean
    is
        use ASCII;
    begin
        return  Char = ' ' or Char = '.' or Char = '\' or Char = '_' or 
                Char = '/' or Char = CR or Char = LF or Char = HT;
    end Is_Word_End;

    -- Determine which direction a selection is based on its start, end, and the
    --  current cursor position.
    function Get_Selection_Direction (st : DAGBuild.GUI.State.UIState) 
        return DAGBuild.GUI.State.Selection_Direction
    is
        use DAGBuild.GUI.State;
    begin
        return Dir : DAGBuild.GUI.State.Selection_Direction do
            if st.Selection_Start = st.Selection_End then
                Dir := NONE;
            elsif st.Cursor_Pos = st.Selection_Start then
                Dir := LEFT;
            else
                Dir := RIGHT;
            end if;
        end return;
    end Get_Selection_Direction;

    -- Return True if the user hit "enter" in this field
    function Text_Field_Navigate (st      : in out DAGBuild.GUI.State.UIState;
                                  Text    : in out Ada.Strings.Unbounded.Unbounded_String)
                                  return Boolean
    is
        package UBS renames Ada.Strings.Unbounded;
        
        use DAGBuild.GUI.State;
        use SDL.Events.Keyboards;
        
        Selection_Dir : constant Selection_Direction := Get_Selection_Direction (st);

        -- Used for ctrl+arrow to skip a word
        procedure Skip_Word_Right (st   : in out DAGBuild.GUI.State.UIState;
                                   Text : Ada.Strings.Unbounded.Unbounded_String)
        is
            Low     : constant Positive := st.Cursor_Pos;
            High    : constant Positive := UBS.Length (Text);
            Char    : Character;
        begin
            Find_Loop: for i in Low .. High loop
                Char := UBS.Element (Text, i);
                st.Cursor_Pos := i;
                exit Find_Loop when Is_Word_End (Char);
            end loop Find_Loop;

            --st.Cursor_Pos := st.Cursor_Pos + 1;
        end Skip_Word_Right;

        procedure Skip_Word_Left (st   : in out DAGBuild.GUI.State.UIState;
                                  Text : Ada.Strings.Unbounded.Unbounded_String)
        is
            High    : constant Positive := st.Cursor_Pos - 1;
            Low     : constant Positive := 2;
            Char    : Character;
        begin
            Find_Loop: for i in reverse Low .. High loop
                Char := UBS.Element (Text, i);
                exit Find_Loop when Is_Word_End (Char);
                st.Cursor_Pos := i;
            end loop Find_Loop;
        end Skip_Word_Left;

    begin
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

                st.Kbd_Pressed := NO_KEY;

                -- Clear cursor and selection
                st.Cursor_Pos := 1;
                st.Selection_Start := 1;
                st.Selection_End := 1;

            when SDL.Events.Keyboards.Code_Return | SDL.Events.Keyboards.Code_KP_Enter =>

                -- Give up keyboard focus for faster entry in the next field.
                st.Kbd_Item := NO_ITEM;
                st.Kbd_Scope := NO_SCOPE;
                st.Kbd_Pressed := NO_KEY;

                st.Cursor_Pos := 1;
                st.Selection_Start := 1;
                st.Selection_End := 1;

                return True;

            when SDL.Events.Keyboards.Code_Left =>
                if st.Cursor_Pos > 1 then

                    if (st.Kbd_Modifier and Modifier_Control) /= 0 then
                        Skip_Word_Left (st, Text);
                    end if;

                    if (st.Kbd_Modifier and Modifier_Shift) /= 0 then
                        if Selection_Dir = LEFT or Selection_Dir = NONE then
                            -- grow selection left
                            st.Cursor_Pos       := st.Cursor_Pos - 1;
                            st.Selection_Start  := st.Cursor_Pos;
                        else
                            -- shrink selection from right
                            st.Cursor_Pos       := st.Cursor_Pos - 1;
                            st.Selection_End    := st.Cursor_Pos;
                        end if;
                    else
                        -- no selection
                        st.Cursor_Pos       := st.Cursor_Pos - 1;
                        st.Selection_Start  := st.Cursor_Pos;
                        st.Selection_End    := st.Selection_Start;
                    end if;
                end if;
            
            when SDL.Events.Keyboards.Code_Right =>
                if st.Cursor_Pos <= UBS.Length(Text) then
                    -- if this is a ctrl+click, then we advance the
                    -- cursor to the next word.
                    if (st.Kbd_Modifier and Modifier_Control) /= 0 then
                        Skip_Word_Right (st, Text);
                    end if;
                    
                    if (st.Kbd_Modifier and Modifier_Shift) /= 0 then
                        if Selection_Dir = RIGHT or Selection_Dir = NONE then
                            -- grow selection right
                            st.Cursor_Pos       := st.Cursor_Pos + 1;
                            st.Selection_End    := st.Cursor_Pos;
                        else
                            -- shrink selection from left
                            st.Cursor_Pos       := st.Cursor_Pos + 1;
                            st.Selection_Start  := st.Cursor_Pos;
                        end if;
                    else
                        -- no selection
                        st.Cursor_Pos       := st.Cursor_Pos + 1;
                        st.Selection_Start  := st.Cursor_Pos;
                        st.Selection_End    := st.Selection_Start;
                    end if;
                end if;

            when SDL.Events.Keyboards.Code_Home =>
                
                if (st.Kbd_Modifier and Modifier_Shift) /= 0 then
                    if Selection_Dir = RIGHT then
                        st.Selection_End    := st.Selection_Start;
                        st.Cursor_Pos       := 1;
                        st.Selection_Start  := st.Cursor_Pos;
                    else
                        st.Selection_End    := st.Cursor_Pos;
                        st.Cursor_Pos       := 1;
                        st.Selection_Start  := st.Cursor_Pos;
                    end if;
                else
                    st.Cursor_Pos           := 1;
                    st.Selection_Start      := st.Cursor_Pos;
                    st.Selection_End        := st.Selection_Start;
                end if;

            when SDL.Events.Keyboards.Code_End =>

                if (st.Kbd_Modifier and Modifier_Shift) /= 0 then
                    if Selection_Dir = LEFT then
                        st.Selection_Start  := st.Selection_End;
                        st.Cursor_Pos       := UBS.Length (Text) + 1;
                        st.Selection_End    := st.Cursor_Pos;
                    else
                        st.Selection_Start  := st.Cursor_Pos;
                        st.Cursor_Pos       := UBS.Length (Text) + 1;
                        st.Selection_End    := st.Cursor_Pos;
                    end if;
                else
                    st.Cursor_Pos := UBS.Length (Text) + 1;
                    st.Selection_Start := st.Cursor_Pos;
                    st.Selection_End := st.Selection_Start;
                end if;

            when SDL.Events.Keyboards.Code_Backspace =>
                if st.Selection_Start < st.Selection_End then
                    UBS.Delete (Text, st.Selection_Start, st.Selection_End - 1);
                    st.Cursor_Pos := st.Selection_Start;
                    st.Selection_End := st.Cursor_Pos;
                else
                    -- no selection, so just delete prev character if there
                    -- is one, and move cursor left.
                    if st.Cursor_Pos > 1 then
                        UBS.Delete (Text, st.Cursor_Pos - 1, st.Cursor_Pos - 1);
                        st.Cursor_Pos       := st.Cursor_Pos - 1;
                        st.Selection_Start  := st.Cursor_Pos;
                        st.Selection_End    := st.Selection_Start;
                    end if;
                end if;

            when SDL.Events.Keyboards.Code_Delete =>
                -- If there's a selection, nuke it and make cursor
                -- go to the start.
                if st.Selection_Start < st.Selection_End then
                    UBS.Delete (Text, st.Selection_Start, st.Selection_End - 1);
                    st.Cursor_Pos       := st.Selection_Start;
                    st.Selection_End    := st.Cursor_Pos;
                else
                    -- no selection, so just delete next character if there
                    -- is one.
                    if st.Cursor_Pos <= UBS.Length (Text) then
                        UBS.Delete (Text, st.Cursor_Pos, st.Cursor_Pos);
                        st.Selection_Start  := st.Cursor_Pos;
                        st.Selection_End    := st.Selection_Start;
                    end if;
                end if;

            when others =>
                --Ada.Text_IO.Put_Line("other: " & st.Kbd_Pressed'Image);
                null;
        end case;

        return False;
    end Text_Field_Navigate;

    -- Handle characters typed or pasted into this field.
    procedure Text_Field_Edit (st           : in out DAGBuild.GUI.State.UIState;
                               Text         : in out Ada.Strings.Unbounded.Unbounded_String;
                               Max_Length   : Natural)
    is
        package UBS renames Ada.Strings.Unbounded;

        Can_Fit     : Integer;
        Select_Len  : constant Natural := st.Selection_End - st.Selection_Start;
        New_Text    : UBS.Unbounded_String;
    begin
        -- determine room to insert/append
        if Max_Length = 0 then
            Can_Fit := UBS.Length (st.Kbd_Text);
        else
            -- If there's a selection that we're about to replace,
            --  then we can add the length of that selection to
            --  the space we have, since it will get wiped out
            --  prior to inserting the new text.
            Can_Fit := Max_Length + Select_Len - UBS.Length (Text);
            
            if Can_Fit < 0 then
                Can_Fit := 0;
            elsif Can_Fit > UBS.Length (st.Kbd_Text) then
                -- Figure out how many chars we can copy here.
                Can_Fit     := UBS.Length (st.Kbd_Text);
                New_Text    := UBS.Unbounded_Slice (Source    => st.Kbd_Text,
                                                    Low       => 1,
                                                    High      => Can_Fit);
            else
                New_Text    := st.Kbd_Text;
            end if;
        end if;

        if Can_Fit > 0 then
            -- If there's a selection, we need to erase it first.
            if st.Selection_Start < st.Selection_End then
                UBS.Delete (Text, st.Selection_Start, st.Selection_End - 1);
                st.Cursor_Pos       := st.Selection_Start;
                st.Selection_End    := st.Cursor_Pos;
            end if;

            if st.Cursor_Pos = UBS.Length (Text) + 1 then
                UBS.Append (Source      => Text,
                            New_Item    => New_Text);
                st.Cursor_Pos       := st.Cursor_Pos + Can_Fit;
                st.Selection_Start  := st.Cursor_Pos;
                st.Selection_End    := st.Selection_Start;
            else
                UBS.Insert (Source      => Text,
                            Before      => st.Cursor_Pos,
                            New_Item    => UBS.To_String (New_Text));
                st.Cursor_Pos       := st.Cursor_Pos + Can_Fit;
                st.Selection_Start  := st.Cursor_Pos;
                st.Selection_End    := st.Selection_Start; 
            end if;

        end if;

        st.Kbd_Text := UBS.Null_Unbounded_String;
    end Text_Field_Edit;

    -- Draw a single line, editable text field
    function Text_Field (st              : in out DAGBuild.GUI.State.UIState;
                         Text            : in out Ada.Strings.Unbounded.Unbounded_String;
                         x               : SDL.Natural_Coordinate;
                         y               : SDL.Natural_Coordinate;
                         Display_Length  : Natural := 20;
                         Max_Length      : Natural := 20) return Boolean
    is
        package UBS renames Ada.Strings.Unbounded;
        use DAGBuild.GUI.State;

        id      : constant DAGBuild.GUI.State.ID := DAGBuild.GUI.State.Next_ID(st);
        scope   : constant DAGBuild.GUI.State.Scope := st.Curr_Scope;
        
        -- Size (in pixels) of the field. @TODO: Need to scale this by ppi
        Field_Width     : constant SDL.Positive_Dimension := SDL.Positive_Dimension(DAGBuild.Settings.Font_Size) * 
                                                                SDL.Positive_Dimension(Display_Length);
        Field_Height    : constant SDL.Positive_Dimension := 2 * SDL.Positive_Dimension(DAGBuild.Settings.Font_Size);
        
        -- How far inside the field to start drawing characters
        Text_Draw_Offset : constant SDL.Positive_Dimension := 4;
        
        Cursor_Click_Update : Boolean := False;
        Cursor_Drag_Update : Boolean := False;

        Hit_Enter : Boolean;

        w : SDL.Dimension;
        h : SDL.Dimension;
    begin

        -- Check for mouseover/click
        if Region_Hit(st, x, y, Field_Width, Field_Height) then
            st.Hot_Item := id;
            st.Hot_Scope := scope;

            -- Update our cursor to the little text dealie
            SDL.Inputs.Mice.Cursors.Set_Cursor(DAGBuild.GUI.State.Text_Cursor);

            -- Clicking on a text field gives us keyboard focus, also 
            --  updates the cursor location.
            if st.Active_Item = NO_ITEM and st.Mouse_Down then
                st.Active_Item  := id;
                st.Active_Scope := scope;
            
                st.Kbd_Item     := id;
                st.Kbd_Scope    := Scope;

                -- Since we got here via a click, we can eventually set the cursor
                --  to the click location. We don't know the details of text rendering
                --  yet in this part of the function, so we'll make a point to
                --  update the cursor later.
                Cursor_Click_Update := True;

                st.Cursor_Pos       := UBS.Length(Text) + 1;
                st.Selection_Start  := st.Cursor_Pos;
                st.Selection_End    := st.Selection_Start;
            end if;

        end if;

        -- if no widget has keyboard focus, take it. 
        if st.Kbd_Item = NO_ITEM then
            st.Kbd_Item     := id;
            st.Kbd_Scope    := Scope;
            
            --We now own the cursor and selection within the UIState, so can
            -- set it to the end of the field.
            st.Cursor_Pos       := UBS.Length(Text) + 1;
            st.Selection_Start  := st.Cursor_Pos;
            st.Selection_End    := st.Selection_Start;
        end if;

        Draw_Rect (st.Renderer,
                   x,
                   y,
                   Field_Width,
                   Field_Height,
                   st.Theme.Input_background);

        Outline_Rect (st.Renderer,
                      x,
                      y,
                      Field_Width,
                      Field_Height,
                      st.Theme.Input_border);

        -- if we have keyboard focus, show it and update heartbeat
        if st.Kbd_Item = id and st.Kbd_Scope = Scope then
            Outline_Rect(st.Renderer,
                        x,
                        y,
                        Field_Width,
                        Field_Height,
                        st.Theme.InputOption_activeBorder);

            st.Kbd_Heartbeat := True;
        end if;

        -- If we are still active, then update cursor based on mouse position
        if st.Active_Item = id and st.Active_Scope = Scope and st.Mouse_Down
            and not Cursor_Click_Update then
            Cursor_Drag_Update := True;
        end if;

        --@TODO Clip the number of characters by what we can actually display.
        Draw_Chars : declare
            -- First_Draw_Index    : Natural;
            -- End_Draw_Index      : Natural;

            Text_x          : SDL.Positive_Dimension := x + Text_Draw_Offset;
            Cursor_x        : SDL.Positive_Dimension := x + Text_Draw_Offset;
            Text_BG_Color   : SDL.Video.Palettes.Colour;
            Text_FG_Color   : SDL.Video.Palettes.Colour;
            Selection_Dir   : Selection_Direction := Get_Selection_Direction(st);
        begin
            if not (st.Kbd_Item = id and st.Kbd_Scope = scope) then
                -- if we aren't active, then just render the text in one fell swoop
                Draw_Text (r        => st.Renderer,
                           Text     => UBS.To_String(Text),
                           x        => Text_x,
                           y        => y + 8,
                           w        => w,
                           h        => h,
                           Color    => st.Theme.Input_Foreground,
                           BG_Color => st.Theme.Input_Background);
            else
                if UBS.Length(Text) > 0 then
                    -- render character-by-character so we can apply
                    -- correct styles to selection and get accurate cursor
                    -- placement.
                    for i in 1 .. UBS.Length(Text) loop

                        if i >= st.Selection_Start and i < st.Selection_End then
                            --Cur_BG_Color := st.Theme.Selection_Background;
                            Text_FG_Color := st.Theme.Input_Background;
                            Text_BG_Color := st.Theme.Input_Foreground;
                        else
                            Text_FG_Color := st.Theme.Input_Foreground;
                            Text_BG_Color := st.Theme.Input_Background;
                        end if;

                        Draw_Text (r     => st.Renderer,
                                Text     => UBS.Slice(Text, i, i),
                                x        => Text_x,
                                y        => y + 8,
                                w        => w,
                                h        => h,
                                Color    => Text_FG_Color,
                                BG_Color => Text_BG_Color);
                    
                        -- if we just clicked here, and we just rendered a character
                        -- whose width takes us past where we clicked, then update
                        -- the cursor position to that character. We need to do
                        -- this here, because otherwise we don't know the position
                        -- each character is drawn at.
                        if Cursor_Click_Update and Text_x >= st.Mouse_x then
                            --Ada.Text_IO.Put_Line ("click" & i'Image);
                            st.Cursor_Pos := i;
                            st.Selection_Start := st.Cursor_Pos;
                            st.Selection_End := st.Selection_Start;
                            Cursor_Click_Update := False;
                        end if;

                        if Cursor_Drag_Update and Text_x >= st.Mouse_x then
                            --Ada.Text_IO.Put_Line ("drag" & i'Image);
                            -- if dragging the mouse, set cursor and adjust the
                            -- selection based on direction.
                            if i > st.Cursor_Pos then
                                --Ada.Text_IO.Put_Line ("left");
                                -- moved cursor left
                                if Selection_Dir = LEFT then
                                    -- grow selection left
                                    st.Cursor_Pos := i;
                                    st.Selection_Start := st.Cursor_Pos;
                                else
                                    -- shrink selection from right
                                    st.Cursor_Pos := i;
                                    st.Selection_End := st.Cursor_Pos;
                                end if;
                            elsif i < st.Cursor_Pos then
                                --Ada.Text_IO.Put_Line ("right");
                                -- moved cursor right
                                if Selection_Dir = RIGHT then
                                    -- grow selection right
                                    st.Cursor_Pos := i;
                                    st.Selection_End := st.Cursor_Pos;
                                else
                                    -- shrink selection from left
                                    st.Cursor_Pos := i;
                                    st.Selection_Start := st.Cursor_Pos;
                                end if;
                            else
                                -- moved cursor none
                                null;
                            end if;
                            Cursor_Drag_Update := False;
                        end if;

                        -- Update the cursor x value when appropriate
                        if st.Cursor_Pos = i then
                            Cursor_x := Text_x;
                        end if;

                        Text_x := Text_x + w;
                    end loop;

                    -- If we haven't updated the Cursor by this point, it
                    --  was to the right of the text we just rendered.
                    if Cursor_Click_Update then
                        st.Cursor_Pos := UBS.Length(Text) + 1;
                        st.Selection_Start := st.Cursor_Pos;
                        st.Selection_End := st.Selection_Start;
                        Cursor_Click_Update := False;
                    end if;

                    if Cursor_Drag_Update then
                        st.Cursor_Pos := UBS.Length(Text) + 1;
                        st.Selection_End := st.Cursor_Pos;
                        Cursor_Drag_Update := False;
                    end if;

                    -- If the cursor is at the end, we won't see it's "character"
                    --  during the above loop. Set it here.
                    if st.Cursor_Pos = UBS.Length (Text) + 1 then
                        Cursor_x := Text_x;
                    end if;
                end if;

                -- Draw the cursor at appropriate position when not blinked
                if st.Blink_On then
                    Draw_Line (r        => st.Renderer,
                               x1       => Cursor_x,
                               y1       => y + 2,
                               x2       => Cursor_x,
                               y2       => y + Field_Height - 2,
                               Color    => st.Theme.EditorCursor_foreground);
                    
                end if;

                -- Expand selection if double clicked.
                if st.Double_Click and UBS.Length (Text) > 0 then
                    Select_Word: declare
                    begin
                        Expand_Start: for i in reverse 1 .. st.Cursor_Pos loop
                            exit Expand_Start when Is_Word_End (UBS.Element (Text, i));
                            st.Selection_Start := i;
                        end loop Expand_Start;

                        Expand_End: for i in st.Cursor_Pos .. UBS.Length (Text) loop
                            exit Expand_End when Is_Word_End (UBS.Element (Text, i));
                            st.Selection_End := i;
                        end loop Expand_End;

                        st.Cursor_Pos := st.Selection_End;
                    end Select_Word;
                end if;
            end if; -- end "if active"
        end Draw_Chars;


        if st.Kbd_Item = id and st.Kbd_Scope = Scope then
            Hit_Enter := Text_Field_Navigate (st, Text);
   
            if Hit_Enter then
                st.Last_Widget := id;
                st.Last_Scope := Scope;

                return True;
            end if;

            if UBS.Length (st.Kbd_Text) /= 0 then
                Text_Field_Edit (st, Text, Max_Length);
            end if;
        end if;

        st.Last_Widget := id;
        st.Last_Scope := Scope;

        return False;
    end Text_Field;

end DAGBuild.GUI.Widgets;

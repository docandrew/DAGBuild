with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with SDL.Events;
with SDL.Events.Keyboards;

with SDL.Video.Renderers;

with DAGBuild.Settings;

package DAGBuild.GUI.State is

    Invalid_Scope_Exception : Exception;

    -- Unique Widget ID
    type ID is new Integer;

    NO_ITEM         : constant ID := 0;
    INVALID_ITEM    : constant ID := -1;

    -- Scope
    type Scope is new Integer range -1..256;

    NO_SCOPE        : constant Scope := 0;
    INVALID_SCOPE   : constant Scope := -1;

    -- Keep track of last assigned IDs in each scope so we can resume once
    -- out of scope.
    type Last_ID_List is array (1..Scope'Last) of ID;

    -- Low-level state of the GUI for rendering and input handling
    -- @field Renderer is the renderer for the app window
    -- @field Mouse_x is the x-position of the mouse
    -- @field Mouse_y is the y-position of the mouse
    -- @field Mouse_Down is True if the mouse button is being held down
    -- @field Hot_Item is the ID of the widget we're hovering over
    -- @field Hot_Scope is the Scope of the widget we're hovering over
    -- @field Active_Item is the ID of the widget we've interacted with
    -- @field Active_Scope is the Scope of the widget we've interacted with
    -- @field Curr_Scope is the current scope we're in
    -- @field Last_IDs is the last ID we generated for a widget in a given scope
    -- @field Kbd_Item is the widget with keyboard focus
    -- @field Kbd_Scope is the scope of the widget with keyboard focus
    -- @field Kbd_Pressed is the key that was pressed
    -- @field Kbd_Modifier is shift, ctrl, alt, etc.
    -- @field Kbd_Heartbeat lets us know if the previously focused widget was
    --  drawn.
    -- @field Kbd_Char is a character entered
    -- @field Kbd_Text should be true if text editing is enabled for active widget.
    -- @field Last_Widget is the ID of the last widget handled
    -- @field Last_Scope is the scope of the last widget handled
    -- @field Done is set to True if we are exiting the program.
    type UIState is
    record
        Renderer        : SDL.Video.Renderers.Renderer;
        Mouse_x         : SDL.Natural_Coordinate;
        Mouse_y         : SDL.Natural_Coordinate;
        Mouse_Down      : Boolean := False;

        Hot_Item        : ID;
        Hot_Scope       : Scope;

        Active_Item     : ID;
        Active_Scope    : Scope;

        Curr_Scope      : Scope := NO_SCOPE;
        Last_IDs        : Last_ID_List := (others => NO_ITEM);

        Kbd_Item        : ID := NO_ITEM;
        Kbd_Scope       : Scope := NO_SCOPE;

        Kbd_Pressed     : SDL.Events.Keyboards.Key_Codes;
        Kbd_Modifier    : SDL.Events.Keyboards.Key_Modifiers;

        Kbd_Heartbeat   : Boolean := False;

        Kbd_Char        : Character;
        Kbd_Text        : Unbounded_String;

        Last_Widget     : ID;
        Last_Scope      : Scope;

        Theme           : DAGBuild.Settings.Color_Scheme := DAGBuild.Settings.Default_Dark;

        Done            : Boolean := False;
    end record;

    -- Extra keyboard help
    NO_KEY : constant SDL.Events.Keyboards.Key_Codes := SDL.Events.Keyboards.Key_Codes(0);

    -- Enter a new Widget scope
    procedure Enter_Scope(st : in out UIState);

    -- Exit a Widget scope
    procedure Exit_Scope(st : in out UIState);

    -- Generate the next ID for the current scope
    function Next_ID(st : in out UIState) return ID;

end DAGBuild.GUI.State;

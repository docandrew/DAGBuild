with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with SDL.Video.Renderers;

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

    -- State of the GUI
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
        Done            : Boolean := False;
    end record;

    -- Enter a new Widget scope
    procedure Enter_Scope(st : in out UIState);

    -- Exit a Widget scope
    procedure Exit_Scope(st : in out UIState);

    -- Generate the next ID for the current scope
    function Next_ID(st : in out UIState) return ID;

end DAGBuild.GUI.State;

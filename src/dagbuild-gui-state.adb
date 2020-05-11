with Interfaces.C; use Interfaces.C;

package body DAGBuild.GUI.State is

    procedure Enter_Scope (st               : in out UIState;
                           Scope_X_Offset   : SDL.Natural_Coordinate := 0;
                           Scope_Y_Offset   : SDL.Natural_Coordinate := 0)
    is
    begin
        if st.Curr_Scope = Scope_ID'Last then
            raise Invalid_Scope_Exception with "Exceeded maximum of nested Enter_Scope calls";
        end if;

        st.Curr_Scope       := st.Curr_Scope + 1;
        st.Scope_X_Offset   := st.Scope_X_Offset + Scope_X_Offset;
        st.Scope_Y_Offset   := st.Scope_Y_Offset + Scope_Y_Offset;
    end Enter_Scope;


    procedure Exit_Scope (st                : in out UIState;
                          Scope_X_Offset    : SDL.Natural_Coordinate := 0;
                          Scope_Y_Offset    : SDL.Natural_Coordinate := 0)
    is
    begin
        if st.Curr_Scope = NO_SCOPE then
            raise Invalid_Scope_Exception with "Called Exit_Scope without matching Enter_Scope";
        end if;

        st.Curr_Scope       := st.Curr_Scope - 1;
        st.Scope_X_Offset   := st.Scope_X_Offset - Scope_X_Offset;
        st.Scope_Y_Offset   := st.Scope_Y_Offset - Scope_Y_Offset;
    end Exit_Scope;

    function Next_ID(st: in out UIState) return ID
    is
        this_ID : constant ID := st.Last_IDs(st.Curr_Scope) + 1;
    begin
        st.Last_IDs(st.Curr_Scope) := this_ID;
        return this_ID;
    end Next_ID;

end DAGBuild.GUI.State;

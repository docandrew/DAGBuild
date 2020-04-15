
package body DAGBuild.GUI.State is

    procedure Enter_Scope(st : in out UIState)
    is
    begin
        if st.Curr_Scope = Scope'Last then
            raise Invalid_Scope_Exception with "Exceeded maximum of 256 nested Enter_Scope calls";
        end if;

        st.Curr_Scope := st.Curr_Scope + 1;
    end Enter_Scope;

    procedure Exit_Scope(st : in out UIState)
    is
    begin
        if st.Curr_Scope = NO_SCOPE then
            raise Invalid_Scope_Exception with "Called Exit_Scope without matching Enter_Scope";
        end if;

        st.Curr_Scope := st.Curr_Scope - 1;
    end Exit_Scope;

    function Next_ID(st: in out UIState) return ID
    is
        this_ID : constant ID := st.Last_IDs(st.Curr_Scope) + 1;
    begin
        st.Last_IDs(st.Curr_Scope) := this_ID;
        return this_ID;
    end Next_ID;

end DAGBuild.GUI.State;
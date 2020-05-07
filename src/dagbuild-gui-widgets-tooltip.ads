with SDL;

package DAGBuild.GUI.Widgets.Tooltip is

    -- Draw a tooltip. It will be shown near the current mouse coordinates,
    --  possibly shifted depending on proximity to the screen edges.
    procedure Tooltip (st : in out DAGBuild.GUI.State.UIState);

end DAGBuild.GUI.Widgets.Tooltip;

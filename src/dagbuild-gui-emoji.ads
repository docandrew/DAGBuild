with Ada.Strings.UTF_Encoding; use Ada.Strings.UTF_Encoding;

package DAGBuild.GUI.Emoji is

    pragma Wide_Character_Encoding(UTF8);

    Smiley_Grin             : constant UTF_String :=    Character'Val(16#F0#) &
                                                        Character'Val(16#9F#) &
                                                        Character'Val(16#98#) &
                                                        Character'Val(16#81#);
    -- Black_Scissors          : constant UTF_8_String := "✂️";
    -- Check_Mark              : constant UTF_8_String := "✔️";
    -- Red_X                   : constant UTF_8_String := "❌";
    -- No_Entry                : constant UTF_8_String := "⛔";
    -- Play_Button             : constant UTF_8_String := "▶️";
    -- Pause_Button            : constant UTF_8_String := "⏸️";
    -- Stop_Button             : constant UTF_8_String := "⏹️";
    -- Clipboard               : constant UTF_8_String := "📋";
    -- Star                    : constant UTF_8_String := "⭐";
    -- Stop_Sign               : constant UTF_8_String := "🛑";
    -- Link                    : constant UTF_8_String := "🔗";
    -- Thumbs_Up               : constant UTF_8_String := "👍";
    -- Bomb                    : constant UTF_8_String := "💣";
    -- Hourglass               : constant UTF_8_String := "⏳";

end DAGBuild.GUI.Emoji;

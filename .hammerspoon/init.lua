local meh = {"ctrl", "alt", "cmd"}
local hyper = {"ctrl", "alt", "cmd", "shift"}

-- Load the editWithEmacs functions
hs.loadSpoon("editWithEmacs")

-- Optional configuration of beginEditShellCommand
spoon.editWithEmacs.openEditorShellCommand = "emacsclient -e '(hammerspoon-edit-begin)' -n"

spoon.editWithEmacs:bindHotkeys({
    selection = {hyper, "e"},
    all = {meh, "e"}
})

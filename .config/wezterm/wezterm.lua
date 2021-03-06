-- WezTerm
-- https://wezfurlong.org/wezterm/index.html

local wezterm = require("wezterm")
local utils = require("utils")

local cfg_misc = {
   window_close_confirmation = "NeverPrompt",
   check_for_updates = false,
   adjust_window_size_when_changing_font_size = false,
   tab_bar_at_bottom = true,
   disable_default_key_bindings = true,
   enable_csi_u_key_encoding = false,
   scrollback_lines = 5000,

   -- Avoid unexpected config breakage and unusable terminal
   automatically_reload_config = false,

   -- Make sure word selection stops on most punctuations.
   -- Note that dot (.) & slash (/) are allowed though for
   -- easy selection of paths.
   selection_word_boundary = " \t\n{}[]()\"'`,;:@│*",

   hide_tab_bar_if_only_one_tab = true,

   -- Do not hold on exit by default.
   -- Because the default 'CloseOnCleanExit' can be annoying when exiting with
   -- Ctrl-D and the last command exited with non-zero: the shell will exit
   -- with non-zero and the terminal would hang until the window is closed manually.
   exit_behavior = "Close",

   -- Pad window to avoid the content to be too close to the border,
   -- so it's easier to see and select.
   window_padding = {
      left = 3, right = 3,
      top = 3, bottom = 3,
   },

  -- Color scheme
  -- https://wezfurlong.org/wezterm/config/appearance.html
 
  color_scheme = 'nord',

  -- window_background_opacity = 0.99,

  -- Font configuration
  -- https://wezfurlong.org/wezterm/config/fonts.html
  font = wezterm.font('PlemolJP Console NF'),
  font_size = 14.0,
  use_ime = true,

  -- Disable ligatures
  -- https://wezfurlong.org/wezterm/config/font-shaping.html
  harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' },

  -- Cursor style
  default_cursor_style = 'BlinkingBar',

  -- Enable CSI u mode
  -- https://wezfurlong.org/wezterm/config/lua/config/enable_csi_u_key_encoding.html
  -- enable_csi_u_key_encoding = true
}

---------------------------------------------------------------
--- keybinds
---------------------------------------------------------------

local my_keybinds = {
	{ key = "5", mods = "LEADER", action = wezterm.action({ SpawnTab = "CurrentPaneDomain" }) },
	{ key = "k", mods = "LEADER|CTRL", action = wezterm.action({ CloseCurrentTab = { confirm = false } }) },
  { key = "k", mods = "LEADER", action = wezterm.action({ CloseCurrentPane = { confirm = false } }) },
	{ key = "[", mods = "SUPER", action = wezterm.action({ ActivateTabRelative = -1 }) },
	{ key = "]", mods = "SUPER", action = wezterm.action({ ActivateTabRelative = 1 }) },
	{ key = "[", mods = "SUPER|SHIFT", action = wezterm.action({ MoveTabRelative = -1 }) },
	{ key = "]", mods = "SUPER|SHIFT", action = wezterm.action({ ActivateTabRelative = 1 }) },
	{ key = "q", mods = "LEADER|CTRL", action = "ActivateCopyMode" },
	{ key = "y", mods = "CTRL", action = wezterm.action({ PasteFrom = "PrimarySelection" }) },
	{ key = "2", mods = "LEADER", action = wezterm.action({ SplitVertical = { domain = "CurrentPaneDomain" } }) },
	{ key = "3", mods = "LEADER", action = wezterm.action({ SplitHorizontal = { domain = "CurrentPaneDomain" } }) },
	{ key = "h", mods = "ALT|SHIFT", action = wezterm.action({ ActivatePaneDirection = "Left" }) },
	{ key = "l", mods = "ALT|SHIFT", action = wezterm.action({ ActivatePaneDirection = "Right" }) },
	{ key = "k", mods = "ALT|SHIFT", action = wezterm.action({ ActivatePaneDirection = "Up" }) },
	{ key = "j", mods = "ALT|SHIFT", action = wezterm.action({ ActivatePaneDirection = "Down" }) },
	{ key = "h", mods = "ALT|SHIFT|CTRL", action = wezterm.action({ AdjustPaneSize = { "Left", 1 } }) },
	{ key = "l", mods = "ALT|SHIFT|CTRL", action = wezterm.action({ AdjustPaneSize = { "Right", 1 } }) },
	{ key = "k", mods = "ALT|SHIFT|CTRL", action = wezterm.action({ AdjustPaneSize = { "Up", 1 } }) },
	{ key = "j", mods = "ALT|SHIFT|CTRL", action = wezterm.action({ AdjustPaneSize = { "Down", 1 } }) },
  { key = "p", mods = "CTRL|ALT", action = wezterm.action({ ScrollByPage = -1 }) },
	{ key = "n", mods = "CTRL|ALT", action = wezterm.action({ ScrollByPage = 1 }) },
  { key = "c", mods = "CTRL|SHIFT", action = wezterm.action({ CopyTo = "Clipboard" }) },
	{ key = "v", mods = "CTRL|SHIFT", action = wezterm.action({ PasteFrom = "Clipboard" }) },
  { key = "=", mods = "CTRL", action = "ResetFontSize" },
	{ key = "+", mods = "CTRL", action = "IncreaseFontSize" },
	{ key = "-", mods = "CTRL", action = "DecreaseFontSize" },
	{ key = " ", mods = "CTRL|SHIFT", action = "QuickSelect" },
	{ key = "r", mods = "ALT", action = "ReloadConfiguration" },
	{ key = "e", mods = "ALT", action = wezterm.action({ EmitEvent = "trigger-emacs-with-scrollback" }) },
	{ key = "s", mods = "CTRL|SHIFT", action = wezterm.action({ Search = { CaseSensitiveString = "" } }) },
}

local function create_keybinds()
   return my_keybinds
end

---------------------------------------------------------------
--- wezterm on
---------------------------------------------------------------
wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
              local title = wezterm.truncate_right(utils.basename(tab.active_pane.foreground_process_name), max_width)
              if title == "" then
                 local uri = utils.convert_home_dir(tab.active_pane.current_working_dir)
                 local basename = utils.basename(uri)
                 if basename == "" then
                    basename = uri
                 end
                 title = wezterm.truncate_right(basename, max_width)
              end
              return {
                 { Text = tab.tab_index + 1 .. ":" .. title },
              }
end)

wezterm.on("update-right-status", function(window, pane)
              local cwd_uri = pane:get_current_working_dir()
              local cwd = ""
              local hostname = ""
              if cwd_uri then
                 cwd_uri = cwd_uri:sub(8)
                 local slash = cwd_uri:find("/")
                 if slash then
                    hostname = cwd_uri:sub(1, slash - 1)
                    -- Remove the domain name portion of the hostname
                    local dot = hostname:find("[.]")
                    if dot then
                       hostname = hostname:sub(1, dot - 1)
                    end
                    if hostname ~= "" then
                       hostname = "@" .. hostname
                    end
                    -- and extract the cwd from the uri
                    cwd = utils.convert_home_dir(cwd)
                 end
              end

              window:set_right_status(wezterm.format({
                                            { Attribute = { Underline = "Single" } },
                                            { Attribute = { Italic = true } },
                                            { Text = cwd .. hostname },
              }))
end)


local io = require("io")
local os = require("os")

wezterm.on("trigger-emacs-with-scrollback", function(window, pane)
              local scrollback = pane:get_lines_as_text(3000)
              local name = os.tmpname()
              local f = io.open(name, "w+")
              f:write(scrollback)
              f:flush()
              f:close()
              window:perform_action(
                 wezterm.action({ SpawnCommandInNewTab = {
                                     args = { "emacs", "-q", "-nw", "-l", "~/.dotfiles/.config/wezterm/empty-buffer-with-skk.el", name },
                 } }),
                 pane
              )
              wezterm.sleep_ms(1000)
              os.remove(name)
end)

---------------------------------------------------------------
--- Config
---------------------------------------------------------------
local config = {
   leader = { key="q", mods="CTRL", timeout_milliseconds=1000 },
   keys = create_keybinds(),
}

return utils.merge_tables(cfg_misc, config, local_config)

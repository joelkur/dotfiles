local wezterm = require 'wezterm'

local config = {}

if wezterm.config_builder then
  config = wezterm.config_builder()
end

local background_color = '#1d2021'
local foreground_color = '#d4be98'
local colors = {
  bg = background_color,
  fg = foreground_color,
  selection_bg = foreground_color,
  selection_fg = background_color,
  cursor = '#a89984',
  cursor_text = background_color,

  c0 = {
    dark = '#665c54',
    light = '#928374',
  },

  c1 = {
    dark = '#ea6962',
    light = '#ea6962',
  },

  c2 = {
    dark = '#a9b665',
    light = '#a9b665',
  },

  c3 = {
    dark = '#e78a4e',
    light = '#d8a657',
  },

  c4 = {
    dark = '#7daea3',
    light = '#7daea3',
  },

  c5 = {
    dark = '#d3869b',
    light = '#d3869b',
  },

  c6 = {
    dark = '#89b482',
    light = '#89b482',
  },

  c7 = {
    dark = '#d4be98',
    light = '#d4be98',
  },
}

config.colors = {
  background = colors.bg,
  foreground = colors.fg,
  selection_fg = colors.selection_fg,
  selection_bg = colors.selection_bg,
  cursor_bg = colors.cursor,
  cursor_fg = colors.cursor_text,

  ansi = {
    colors.c0.dark,
    colors.c1.dark,
    colors.c2.dark,
    colors.c3.dark,
    colors.c4.dark,
    colors.c5.dark,
    colors.c6.dark,
    colors.c7.dark,
  },
  brights = {
    colors.c0.light,
    colors.c1.light,
    colors.c2.light,
    colors.c3.light,
    colors.c4.light,
    colors.c5.light,
    colors.c6.light,
    colors.c7.light,
  },
}

config.font_size = 16
config.font = wezterm.font 'IosevkaTerm Nerd Font Mono'

config.hide_tab_bar_if_only_one_tab = true

config.audible_bell = "Disabled"

return config

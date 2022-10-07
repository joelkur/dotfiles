local ls = require("luasnip")
local snip = ls.snippet
local node = ls.snippet_node
local text = ls.text_node
local insert = ls.insert_node
local func = ls.function_node
local choice = ls.choice_node
local dynamic = ls.dynamic_node

ls.add_snippets("typescriptreact", {
	snip({
		trig = "ujsx",
		namr = "Use jsx in fresh component",
		dscr = "Tells fresh that jsx is used in this file",
	}, {
		text({ "/** @jsx h */", 'import { h } from "preact"' }),
	}),
})

ls.add_snippets("typescriptreact", {
	snip({
		trig = "tw",
		namr = "Twind class",
		dscr = "Twind class definition",
	}, {
		text("class={tw`"),
		insert(1, ""),
		text("`}"),
	}),
})

-- chat.lua — group :::user / :::gpt blocks into a chat container
local function hasclass(div, cls)
  for _, c in ipairs(div.classes) do if c == cls then return true end end
  return false
end

local function is_chat(div)
  return div.t == "Div" and (hasclass(div, "user") or hasclass(div, "gpt") or hasclass(div, "assistant"))
end

local function map_chat(div)
  -- turn .user -> .chat-user, .gpt/.assistant -> .chat-gpt
  local new = {}
  if hasclass(div, "user") then table.insert(new, "chat-user") end
  if hasclass(div, "gpt") or hasclass(div, "assistant") then table.insert(new, "chat-gpt") end
  -- keep other classes (e.g., fragment, card)
  for _, c in ipairs(div.classes) do
    if c ~= "user" and c ~= "gpt" and c ~= "assistant" then
      table.insert(new, c)
    end
  end
  div.classes = new
  return div
end

local function group_blocks(blocks)
  local out, i = pandoc.List(), 1
  while i <= #blocks do
    local b = blocks[i]
    if b.t == "Div" and is_chat(b) then
      local kids = pandoc.List()
      while i <= #blocks and blocks[i].t == "Div" and is_chat(blocks[i]) do
        kids:insert(map_chat(blocks[i]))
        i = i + 1
      end
      out:insert(pandoc.Div(kids, {class = "chat-container"}))
    else
      out:insert(b)
      i = i + 1
    end
  end
  return out
end

-- run grouping at document level and inside any Div/Section
function Pandoc(doc)
  doc.blocks = group_blocks(doc.blocks)
  return doc
end

function Div(el)
  el.content = group_blocks(el.content)
  return el
end

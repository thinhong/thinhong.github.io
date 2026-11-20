-- Blog posts take the colour of their topic group.
-- A post's topic is its first category. The map from topic to colour is `topics`
-- in blog/_metadata.yml (see "Colour in the blog" in STYLE-GUIDE.md).
-- The page colour of a blog post is set by its body class (hue-ma, mauve). Quarto
-- applies body-classes before filters run, so this filter adds one CSS line to the
-- page head instead: it points the page colour (--hue, --hue-bg, --hue-fg) at the
-- topic's group. The title rule, contents dot, callout tint and topic pill follow it.
-- A post with no known topic keeps mauve.

local function text(v)
  return v and pandoc.utils.stringify(v) or ""
end

function Meta(meta)
  local topics, cats = meta.topics, meta.categories
  if not topics or not cats or not quarto.doc.is_format("html") then
    return nil
  end
  local first = pandoc.utils.type(cats) == "List" and cats[1] or cats
  local topic = topics[text(first):lower()]
  local hue = topic and text(topic.hue) or ""
  if not hue:match("^%a%a$") then
    return nil
  end
  quarto.doc.include_text("in-header", string.format(
    "<style>body.sec-blog{--hue:var(--%s);--hue-bg:var(--%s-bg);--hue-fg:var(--%s-fg)}</style>", hue, hue, hue))
  return nil
end

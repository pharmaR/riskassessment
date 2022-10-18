function RawBlock (raw)
  if raw.format:match 'html' and not FORMAT:match 'html' then
    return pandoc.read(raw.text, raw.format).blocks
  end
end
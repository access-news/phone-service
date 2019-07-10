local f = {}

function f.speak(...) -- {{{2

  text = ""

  for _index, note in ipairs({...}) do
    text = text .. " " .. note
  end

  session:execute("speak", "flite|slt|" .. text)
end

function f.silence(s) -- {{{2
  session:execute("playback", "silence_stream://" .. tostring(s) .. ",1400")
end


return f

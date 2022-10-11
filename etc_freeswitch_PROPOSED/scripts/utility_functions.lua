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

function f.sched_hangup() -- {{{2
  session:execute("sched_transfer", "+600 9999 XML default")
end

function f.abort_sched_hangup() -- {{{2
  local lsi = session:getVariable("last_sched_id")
  session:execute("sched_cancel", tostring(lsi))
end

return f

-- vim: set fdm=marker:

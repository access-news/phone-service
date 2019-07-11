local d = require("db_queries")
local f = require("utility_functions")
local i = require("ivr")

--- Call flow ---------------------------------------------------------- {{{1

session:answer()

-- NOTE: Not sure  which one is  better, but it is  needed to
--       give some  time to the  system to start  playing the
--       menu (either a file, `phrase`, `say`, `speak`, etc.)
-- session:execute("sleep", "750")
f.silence(750)

--- Is caller registered? ---------------------------------------------- {{{2
if d.ani_registered() == false then

  f.sched_hangup()
  i.unregistered_main:execute(session, "unregistered_main")

  -- The  control goes  to `ivr.lua`  (look for  the line
  -- with `login.lua`).  When user enters  their security
  -- code, `login.lua` will get invoked.
  --
  -- If the code is correct: scheduled hangup will be canceled, and main menu will start.
  -- If incorrect: announcement, jump to main menu, and, eventually, scheduled hangup.
else
  i.main:execute(session, "main")
end

-- vim: set fdm=marker:

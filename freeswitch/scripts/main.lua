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

  -- session:execute("sched_hangup", "+600 allotted_timeout")
  session:execute("sched_transfer", "+600 9999 XML default")

  i.unregistered_main:execute(session, "unregistered_main")
else
  i.main:execute(session, "main")
end

-- vim: set fdm=marker:

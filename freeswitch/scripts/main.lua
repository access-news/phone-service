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

-- session:execute("speak", "flite|slt|" .. welcome_note)
-- speak(welcome_note)

--- Is caller registered? ---------------------------------------------- {{{2
if d.ani_registered() == false then

  -- speak(demo_note, register_note)

  -- This will  schedule a  hangup after 10  minutes, but
  -- this would  kick out the  user even if  they provide
  -- their passcode.
  --
  -- TODO: Implement a timer than can be reset when passcode is
  --       supplied.
  session:execute("sched_hangup", "+600 allotted_timeout")

  i.unregistered_main:execute(session, "unregistered_main")
else
  i.main:execute(session, "main")
end

-- vim: set fdm=marker:

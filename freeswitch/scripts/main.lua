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

-- WHY SO MANY MENUS?
--
-- 1. Why not use `speak` for specific parts?
--
--    `ivr.lua` and `./freeswitch/phrases/lang/en/tr2.xml`
--    may seem unnecessarily  complicated, but the problem
--    with  `speak` is  that  if a  user starts  inputting
--    their code then it will get lost in the aether.
--
-- 2. What about `play_and_get_digits`?
--
--    I think it would be overly complicated to handle the
--    extra  variable or  the  tranisition  when the  user
--    didn't finish  entering the code but  the menu kicks
--    in.

-- GENERAL FLOW
--
--    ____`main.lua`_____             ____`ivr.lua`______
--   /                   \           |                   |
--  < Is user registered? > - yes -> | i.registered_main |
--   \_________ _________/           |_________|_________|
--             |                              (|)
--             |                              (|)
--             |                              (V)
--             |                   ____`ivr.lua`______
--            no                  |                   |
--             |                  | i.common_menu     |<-*
--             |                  |___________________|  |
--             |                                         |
--             V                                         |
--    ____`ivr.lua`__________       ____`login.lua`__   yes
--   |                       |     /                 \   |
--   | i.unregistered_main -----> < Code checks out?  >-*
--   |_______________________|     \________ ________/
--                       ^                  |
--                       |_____no___________|

if d.ani_registered() == false then

  f.sched_hangup()
  i.unregistered_main:execute(session, i.unregistered_main_name)

  -- The  control goes  to `ivr.lua`  (look for  the line
  -- with `login.lua`).  When user enters  their security
  -- code, `login.lua` will get invoked.
  --
  -- If the code is correct: scheduled hangup will be canceled, and main menu will start.
  -- If incorrect: announcement, jump to main menu, and, eventually, scheduled hangup.
else
  i.main:execute(session, i.registered_main_name)
end

-- vim: set fdm=marker softtabstop=2 shiftwidth=2 tabstop=2 expandtab:

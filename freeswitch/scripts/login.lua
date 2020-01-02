local d = require("db_queries")
local f = require("utility_functions")
local i = require("ivr")

if d.check_sec_code(argv[1]) == true then
  -- freeswitch.consoleLog("info", "YAY")
  f.abort_sched_hangup()
  f.speak("Thank you for logging in!")

  -- WHY IS CALLING `common_menu` NECESSARY?
  --
  -- See general  flow in  `main.lua`, but the  answer is
  -- because when unregistered user calls then the system
  -- starts  `unregistered_main` IVR  menu (where  we are
  -- now)  and its  `digit_length`  is 10  instead of  1
  -- (like  `registered_main`'s)  and  that can  mess  up
  -- things (see `ivr.lua` for the menu implementations).
  i.common_menu:execute(session, i.common_menu_name)
else
  -- WHY IS `common_menu` NOT CALLED HERE?
  --
  -- Because    control    needs    to   go    back    to
  -- `unregistered_main`   to   check  subsequent   login
  -- attempts.
  f.speak("Security code incorrect.")
end



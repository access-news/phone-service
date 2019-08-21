local d = require("db_queries")
local f = require("utility_functions")
local i = require("ivr")

if d.check_sec_code(argv[1]) == true then
  -- freeswitch.consoleLog("info", "YAY")
  f.abort_sched_hangup()
  f.speak("Thank you for logging in!")

  -- TODO: is this really needed?
  i.main:execute(session, "main")
else
  -- freeswitch.consoleLog("CRIT", "nay...")
  f.speak("Security code invalid.")
end


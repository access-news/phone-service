local d = require("db_queries")
local f = require("utility_functions")

if d.check_sec_code(argv[1]) == true then
  -- freeswitch.consoleLog("info", "YAY")
  f.speak("Thank you for logging in!")
else
  -- freeswitch.consoleLog("CRIT", "nay...")
  f.speak("Security code invalid.")
end


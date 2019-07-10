freeswitch.consoleLog("CRIT", tostring(argv[1]))

local d = require("db_queries")

if d.check_sec_code(argv[1]) == true then
  freeswitch.consoleLog("info", "YAY")
else
  freeswitch.consoleLog("CRIT", "nay...")
end


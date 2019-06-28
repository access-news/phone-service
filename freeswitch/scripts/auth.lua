--- Globals ------------------------------------------------------------ {{{1

ani = session:getVariable("ani")
-- freeswitch.consoleLog("INFO", "lua script: ani = " .. ani)

-- TODO: let's not advertize credentials once in prod. See TODOs in README.
conn_string =
  "pgsql://hostaddr=10.142.0.2"              ..
  " dbname=access-news"                      ..
  " user=postgres"                           ..
  " password=postgres"                       ..
  " options='-c client_min_messages=NOTICE'" ..
  " application_name='freeswitch'"

dbh = freeswitch.Dbh(conn_string)
assert(dbh:connected())
-- freeswitch.consoleLog("INFO", "lua script: connected to DB")

--- Functions ---------------------------------------------------------- {{{1

-- TODO: It would probably be prudent to put these in a module.
-- TODO: `ani_registered()` and `check_sec_code()` are almost the same

function ani_registered(ani) -- {{{2

  local registered = false

  local q =
    "SELECT '+1' || phone_number"                    ..
    "  FROM phone_numbers"                           ..
    "  WHERE '+1' || phone_number = '" .. ani .. "'"

  dbh:query(q, function(row)
    registered = true
  end)

  return registered
end

function check_sec_code(digits) -- {{{2

  local authenticated = false

  local q =
    "SELECT  phone_number"     ..
    "  FROM  phone_numbers"    ..
    "  WHERE phone_number = '" .. digits .. "'"

  dbh:query(q, function(row)
    authenticated = true
  end)

  return authenticated
end

--- Call flow ---------------------------------------------------------- {{{1

session:answer()

-- NOTE: Not sure  which one is  better, but it is  needed to
--       give some  time to the  system to start  playing the
--       menu (either a file, `phrase`, `say`, `speak`, etc.)
-- session:execute("sleep", "750")
session:execute("playback", "silence_stream://750,1400")

session:execute("speak", "flite|slt|Welcome to Access News!")

if ani_registered(ani) == false then
  freeswitch.consoleLog("INFO", ani .. " is not registered")

  local not_registered_note =
    "The number you "
  session:execute("speak", "flite|slt|" .. demo_note)

  -- NOTE: `flite` is not aware  of punctuation marks therefore
  -- the entire  text is read  in one long string.  It is
  -- just a placeholder right now anyway.
  local demo_note =
    "Please note that you are currently in demo"           ..
    "mode and the system will hang up in 5 minutes."       ..
    "You can register any time by calling 916, 889, 7519."

  session:execute("speak", "flite|slt|" .. demo_note)
  session:execute("sched_hangup", "+300 allotted_timeout")
end

freeswitch.consoleLog("INFO", ani .. " registered")

session:execute("sleep", "750")
session:execute("ivr","demo_ivr")

dbh:release()

-- vim: set fdm=marker:

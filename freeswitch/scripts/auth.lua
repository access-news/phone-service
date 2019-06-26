ani = session:getVariable("ani")
freeswitch.consoleLog("INFO", "lua script: ani = " .. ani)

-- TODO: let's not advertize credentials once in prod
-- + https://www.digitalocean.com/community/tutorials/an-introduction-to-managing-secrets-safely-with-version-control-systems
-- + https://news.ycombinator.com/item?id=5178914
-- + https://github.com/google/tink
-- + https://johnresig.com/blog/keeping-passwords-in-source-control/
-- + https://www.agwa.name/projects/git-crypt/
conn_string =
  "pgsql://hostaddr=10.142.0.2"              ..
  " dbname=access-news"                      ..
  " user=postgres"                           ..
  " password=postgres"                       ..
  " options='-c client_min_messages=NOTICE'" ..
  " application_name='freeswitch'"

dbh = freeswitch.Dbh(conn_string)

assert(dbh:connected())
freeswitch.consoleLog("INFO", "lua script: connected to DB")

session:execute("sleep", "750")
session:execute("speak", "flite|slt|Welcome to Access News!")

function ani_registered(ani)

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

if ani_registered(ani) == false then
  freeswitch.consoleLog("INFO", ani .. " is not registered")

  -- TODO: ask for the PIN at this point (last 4 digits
  -- of registered number) and if that fails, then kick
  -- into demo mode

  -- flite is not aware of punctuation marks therefore
  -- the entire text is read in one long string, but it
  -- is just a placeholder right now anyway.
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

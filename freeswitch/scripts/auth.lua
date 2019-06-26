
ani = session:getVariable("ani")

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

-- NOTE: Workaround for when query returns no rows.
--       See https://stackoverflow.com/questions/56744392
q =
  "SELECT COALESCE("                                  ..
  "  (SELECT '+1' || phone_number"                    .. -- \
  "    FROM phone_numbers"                            .. --  > subquery
  "    WHERE '+1' || phone_number = '" .. ani .. "')" .. -- /
  "  , 'unauthorized'"                                .. -- return "unauthorized" if no matching rows
  ")"

session:execute("sleep", "750")
session:execute("speak", "flite|slt|Welcome to Access News!")

dbh:query(q, function(row)

  freeswitch.consoleLog("INFO", "lua script: ani = " .. ani)

  if row.coalesce == "unauthorized" then
    freeswitch.consoleLog("INFO", "lua script: " .. ani .. " not in database, hanging up")

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
    -- session:hangup()
  end

  session:execute("sleep", "750")
  session:execute("ivr","demo_ivr")

end)

dbh:release()


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
  "SELECT COALESCE(" ..
    "(SELECT '+1' || phone_number " ..
      "FROM phone_numbers " ..
      "WHERE '+1' || phone_number = '" .. ani .. "')" .. 
    ", '0'" ..
  ")"

dbh:query(q, function(row)

  freeswitch.consoleLog("INFO", "lua script: ani = " .. ani)

  if row.coalesce == "0" then
    freeswitch.consoleLog("INFO", "lua script: " .. ani .. " not in database, hanging up")
    session:hangup()
  end

  session:execute("sleep", "750")
  session:execute("ivr","demo_ivr")

end)

dbh:release()

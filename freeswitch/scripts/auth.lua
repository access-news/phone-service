-- dbh2 = freeswitch.Dbh("sqlite://core") -- sqlite database in subdirectory "db"
-- assert(dbh2:connected()) -- exits the script if we didn't connect properly
-- dbh2:query("SELECT name,filename FROM interfaces WHERE name='luarun'", function(row)
--   stream:write("name is ".. row.name .. " while module is " .. row.filename)
-- end)

ani = session:getVariable("ani")
dbh = freeswitch.Dbh("pgsql://hostaddr=10.142.0.2 dbname=access-news user=postgres password=postgres options='-c client_min_messages=NOTICE' application_name='freeswitch'")

assert(dbh:connected())
freeswitch.consoleLog("INFO", "lua script: connected to DB")

-- number = "+18083218032"

q =
  "SELECT COALESCE(" ..
    "(SELECT '+1' || phone_number " ..
      "FROM phone_numbers " ..
      "WHERE '+1' || phone_number = '" .. ani .. "')" .. 
    ", '0'" ..
  ")"

dbh:query(q, function(row)
-- x = pcall(dbh:query("SELECT '+1' || phone_number FROM phone_numbers WHERE '+1' || phone_number = '" .. number .. "'", function(row)
  -- stream:write(string.format("%s\n", type(row)))

  -- for key, val in pairs(row) do
  --   stream:write("--- sor ---")
  --   stream:write(string.format("%5s : %s\n", key,  val))
  -- end

  freeswitch.consoleLog("INFO", "lua script: ani = " .. ani)

  if row.coalesce == "0" then
    freeswitch.consoleLog("INFO", "lua script: " .. ani .. " not in database")
    session:hangup()
  end
  --   freeswitch.consoleLog("CRIT", "true")
  -- else
  -- if next(row) == nil then
    -- freeswitch.consoleLog("INFO", "empty")
  -- end

end)

-- stream:write(string.format("%s\n", type(x)))

dbh:release()

-- if not ( ani == "+18083218036" or ani == "+19168897510" ) then
--   session:hangup()
-- end

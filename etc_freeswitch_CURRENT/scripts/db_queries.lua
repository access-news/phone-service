local d = {}

--- private functions ---------------------------------------------- {{{1

function tr2_connect() -- {{{2

  -- See README 0.0.1 for "conn_string"
  local c = require "conn_string"
  local dbh = freeswitch.Dbh(c.conn_string)

  assert(dbh:connected())

  return dbh
end

function bool_query(query_string) -- {{{2

  local dbh    = tr2_connect()
  local status = false

  dbh:query(q, function(row)
    status = true
  end)

  return status
end

--- module functions ---------------------------------------------- {{{1

function d.ani_registered() -- {{{2

  local ani = session:getVariable("ani")
  local dbh = tr2_connect()

  local registered = false

  -- don't put `local` before this var
  q =
    "SELECT '+1' || phone_number"                    ..
    "  FROM phone_numbers"                           ..
    "  WHERE '+1' || phone_number = '" .. ani .. "'"

  dbh:query(q, function(row)
    registered = true
  end)

  return registered
end

function d.check_sec_code(digits) -- {{{2

  -- don't put `local` before this var
  q =
    "SELECT  phone_number"     ..
    "  FROM  phone_numbers"    ..
    "  WHERE phone_number = '" .. digits .. "'"

  return bool_query(q)
end

return d

-- vim: set fdm=marker:

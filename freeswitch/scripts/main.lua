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

function silence(s) -- {{{2
  session:execute("playback", "silence_stream://" .. tostring(s) .. ",1400")
end

function demo_warning() -- {{{2
  -- NOTE: `flite` is not aware  of punctuation marks therefore
  -- the entire  text is read  in one long String.  It is
  -- just a placeholder right now anyway.
  local demo_note =
    "Please note that you are currently in demo mode. The" ..
    "system will  hang up after  10 minutes, but  you can" ..
    "log in  any time if  you are already  registered, by" ..
    "pressing star."

  local register_note =
    "If you would like to sign up for the service, please" ..
    "call " .. access_news_phone_number

  -- TODO: Implement leaving a message (by pressing 0, for example).
  --
  --       How would that be sent  to admins? For example email
  --       the audio  as an attachment with  a transcription as
  --       email body.

  session:execute("speak", "flite|slt|" .. demo_note .. " " .. register_note)
end

function call_IVRMenu(tbl) -- {{{2
  -- is return needed, or is it implicit in lua for the last statement?
  return freeswitch.IVRMenu(
    tbl["main"                ],
    tbl["name"                ],
    tbl["greet_long"          ],
    tbl["greet_short"         ],
    tbl["invalid_sound"       ],
    tbl["exit_sound"          ],
    tbl["transfer_sound"      ],
    tbl["confirm_macro"       ],
    tbl["confirm_key"         ],
    tbl["tts_engine"          ],
    tbl["tts_voice"           ],
    tbl["confirm_attempts"    ],
    tbl["inter_digit_timeout" ],
    tbl["digit_len"           ],
    tbl["timeout"             ],
    tbl["max_failures"        ],
    tbl["max_timeouts"        ]
  );
end

--- IVR menus ---------------------------------------------------------- {{{1

-------------------------------------
-- `freeswitch.IVRMenu()` options --
------------------------------------
-- main,                  ?IVRMenu: the top level menu or nil if this is the top level one
-- name,                  ?string:  the name of the menu
-- greeting_sound,        ?string:  the menu prompt played the first time the menu is played
-- short_greeting_sound,  ?string:  the short version of the menu prompt played on subsequent loops
-- invalid_sound,         ?string:  the sound to play when an invalid entry/no entry is made
-- exit_sound,            ?string:  played when the menu is terminated
-- transfer_sound,        ?string:  the sound to play on transfer
-- confirm_macro,         ?string:  phrase macro name to confirm input
-- confirm_key,           ?string:  the key used to confirm a digit entry
-- tts_engine,            ?string:  the TTS engine to use for this menu
-- tts_voice,             ?string:  the TTS voice to use for this menu
-- confirm_attempts,      ?int:     number of times to prompt to confirm input before failure,
--                                  -1 for unlimited
-- inter_timeout,         ?int:     inter-digit timeout
-- digit_len,             ?int:     max number of digits
-- timeout,               ?int:     number of milliseconds to pause before looping
-- max_failures,          ?int:     threshold of failures before hanging up
-- max_timeouts)          ?int:     threshold of timeouts before hanging up,
--                                  -1 for unlimited?

main_opts = {
  ["main"]                = nil,
  ["name"]                = "tr2_main",
  ["greet_long"]          = "phrase:main_menu",
  ["greet_short"]         = "phrase:main_menu",
  ["invalid_sound"]       = "ivr/ivr-that_was_an_invalid_entry.wav",
  ["exit_sound"]          = "voicemail/vm-goodbye.wav",
  ["transfer_sound"]      = nil,
  ["confirm_macro"]       = nil,
  ["confirm_key"]         = nil,
  ["tts_engine"]          = "flite",
  ["tts_voice"]           = "rms",
  ["confirm_attempts"]    = "3",
  ["inter_digit_timeout"] = "2000",
  ["digit_len"]           = "10",
  ["timeout"]             = "10000",
  ["max_failures"]        = "3",
  ["max_timeouts"]        = "2"
}

-- The only difference between  this and `main_opts` is
-- by  adding the  word  "unregistered"  to the  marked
-- lines.
-- (Lua  tables are  objects,  and  only the  reference
-- gets  copied, hence  the duplication  of the  entire
-- structure.  Could've gone  the way  of mutating  one
-- object, but  that would've gotten me  into all sorts
-- of messes.)
unregistered_main_opts = {
  ["main"]                = nil,
  ["name"]                = "unregistered_tr2_main",          -- *
  ["greet_long"]          = "phrase:unregistered_main_menu",  -- *
  ["greet_short"]         = "phrase:unregistered_main_menu",  -- *
  ["invalid_sound"]       = "ivr/ivr-that_was_an_invalid_entry.wav",
  ["exit_sound"]          = "voicemail/vm-goodbye.wav",
  ["transfer_sound"]      = nil,
  ["confirm_macro"]       = nil,
  ["confirm_key"]         = nil,
  ["tts_engine"]          = "flite",
  ["tts_voice"]           = "rms",
  ["confirm_attempts"]    = "3",
  ["inter_digit_timeout"] = "2000",
  ["digit_len"]           = "1",
  ["timeout"]             = "10000",
  ["max_failures"]        = "3",
  ["max_timeouts"]        = "2"
}

--- Call flow ---------------------------------------------------------- {{{1

session:answer()

top = call_IVRMenu(main_opts)
-- top:bindAction("menu-exec-app", "echo", "1")
top:bindAction("menu-exec-app", "info", "1")
top:bindAction("menu-exec-app", "info", "2")
top:bindAction("menu-exec-app", "lua t.lua $1", "/^([0-9]{10})$/")
-- top:bindAction("menu-exec-app", "lua /home/toraritte/clones/TR2/t.lua \"$1\"", "/^(1234567890)$/")
-- top:bindAction("menu-exec-app", "lua ~freeswitch.consoleLog(\"CRIT\", $1)", "/^1234567890$/")
top:execute(session, "tr2_main")

---- NOTE: Not sure  which one is  better, but it is  needed to
----       give some  time to the  system to start  playing the
----       menu (either a file, `phrase`, `say`, `speak`, etc.)
---- session:execute("sleep", "750")
--silence(750)

--local access_news_phone_number = "916 889 7519"

--local welcome_note =
--  "Welcome to Access News, a service of Society For The" ..
--  "Blind!  If  you  have any  questions,  concerns,  or" ..
--  "suggestions, please call us at " .. access_news_phone_number

--session:execute("speak", "flite|slt|" .. welcome_note)

----- Is caller registered? ---------------------------------------------- {{{2
--if ani_registered(ani) == false then
--  freeswitch.consoleLog("INFO", ani .. " is not registered")

--  demo_warning()

--  -- This will  schedule a  hangup after 10  minutes, but
--  -- this would  kick out the  user even if  they provide
--  -- their passcode.
--  --
--  -- TODO: Implement a timer than can be reset when passcode is
--  --       supplied.
--  session:execute("sched_hangup", "+600 allotted_timeout")
--end

--freeswitch.consoleLog("INFO", ani .. " registered")

--silence(750)
---- session:execute("ivr","demo_ivr")

---- local main = freeswitch.IVRMenu(
dbh:release()

-- vim: set fdm=marker:

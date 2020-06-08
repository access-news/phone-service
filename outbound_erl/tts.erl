-module(tts).

-export(
    [
      google_TTS_to_wav/1
    ]).

% String -> String -> Integer (0 - 400)
google_TTS_to_wav
({ Text
 , FileBasename
 , Speed
 }
)
  when Speed >= 1
     , Speed =< 400
->
    os:cmd(
           "./google-tts-wav.sh"
        ++ " \"" ++ Text ++ "\" "
        % ++ "0.87 > "
        ++ futil:stringify(Speed/100)
        ++ " > "
        ++ menus:get_audiofilename(FileBasename, Speed)
    ).

% vim: set fdm=marker:
% vim: set foldmarker={{-,}}-:
% vim: set nowrap:

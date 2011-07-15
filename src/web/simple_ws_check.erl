-module (simple_ws_check).  
-export ([is_acceptable/6]).
-include("ejabberd.hrl").
is_acceptable(["true"]=Path, Q, Origin, Protocol, IP, Headers)->
    ?INFO_MSG("Authorized Websocket ~p with: ~n Q = ~p~n Origin = ~p~n Protocol = ~p~n IP = ~p~n Headers = ~p~n", 
        [Path, Q, Origin, Protocol, IP, Headers]),
    true;
is_acceptable(["false"]=Path, Q, Origin, Protocol, IP, Headers)->
    ?INFO_MSG("Failed Websocket ~p with: ~n Q = ~p~n Origin = ~p~n Protocol = ~p~n IP = ~p~n Headers = ~p~n", 
        [Path, Q, Origin, Protocol, IP, Headers]),
    false.
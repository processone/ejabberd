-module(mod_muc_hook).

-export([room_state/3,
		room_user/5,
		room_message/5,
		room_message_private/7]). 


room_state(Host, Room, State) ->
	ok.

room_user(Host, Room, JID, Nick, State) ->
	ok.

room_message(Host, Room, FromJID, _FromNick, Message) ->
	Message.

room_message_private(Host, Room, FromJID, _FromNick, ToJIDs, _ToNick, Message) ->
	Message.

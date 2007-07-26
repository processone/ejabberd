-record(privacy, {us,
		  default = none,
		  lists = []}).

-record(listitem, {type = none,
		   value = none,
		   action,
		   order,
		   match_all = false,
		   match_iq = false,
		   match_message = false,
		   match_presence_in = false,
		   match_presence_out = false
		  }).

-record(userlist, {name = none, list = []}).

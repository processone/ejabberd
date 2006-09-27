-record(adhoc_request, {lang,
			node,
			sessionid,
			action,
			xdata,
			others}).

-record(adhoc_response, {lang,
			 node,
			 sessionid,
			 status,
			 defaultaction = "",
			 actions = [],
			 notes = [],
			 elements = []}).

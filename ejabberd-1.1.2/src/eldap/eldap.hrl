-record(eldap_search, {scope = wholeSubtree,
		       base = [],
		       filter,
		       attributes = [],
		       types_only = false,
		       timeout = 0}).


-record(eldap_search_result, {entries,
			      referrals}).

-record(eldap_entry, {object_name,
		      attributes}).

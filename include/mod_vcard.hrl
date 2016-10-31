-record(vcard_search,
	{us, user, luser, fn, lfn, family, lfamily, given,
	 lgiven, middle, lmiddle, nickname, lnickname, bday,
	 lbday, ctry, lctry, locality, llocality, email, lemail,
	 orgname, lorgname, orgunit, lorgunit}).

-record(vcard, {us = {<<"">>, <<"">>} :: {binary(), binary()} | binary(),
                vcard = #xmlel{} :: xmlel()}).

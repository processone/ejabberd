%%%----------------------------------------------------------------------
%%% File    : ejabberd_http.hrl
%%% Author  : Alexey Shchepin <alexey@sevcom.net>
%%% Purpose :
%%% Created :  4 Mar 2004 by Alexey Shchepin <alexey@sevcom.net>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

-record(request, {method,
		  path,
		  q = [],
		  us,
		  auth,
		  lang = "",
		  content_type,
		  data = "",
		  ip
		 }).

-record(http_data, {content_type,
		    content_disposition,
		    args=[],
		    data
		   }).

%%%  This code was developped by IDEALX (http://IDEALX.org/) and
%%%  contributors (their names can be found in the CONTRIBUTORS file).
%%%  Copyright (C) 2000-2001 IDEALX
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%
%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two.


-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

%% use by the client to create the request
-record(http_request, {
          url,
          version="1.1", % default is HTTP/1.1
          host_header,   % use for the 'Host:' header
          get_ims_date,  % used when the method is getims
          cookie = [],
          method = get,
          content_type = [],
          headers = [],
          body = [],
          id = 0,
          user_agent,
          userid, % for www_authentication
          passwd, % for www_authentication
          soap_action % for SOAP support
         }).

-record(http_dyndata,
        {
          user_agent,
          cookies = [] % HTTP Cookies
         }
       ).

-record(url,
        {scheme,          %% http, https, ...
         host,
         port,            %% undefined means use default (80 or 443)
         path = [],
         querypart = []}).

%% use by the client process to store information about the current request during
%% the parsing of the response
-record(http, {content_length= 0,  % HTTP header: content length
               body_size     = 0,  % current size of body,
               chunk_toread  = -1, % chunk data to be read (-1 = not chunked)
               status        = none, % HTTP resp. status :200, etc. 'none'
                                     % if no current cnx.
               close         = false, % true if HTTP/1.0 or 'connection: close'
                                     % has been received
               partial=false,    % true if headers are partially received
               cookie=[]
              }).


-record(cookie,{
          key,
          value,
          quoted,
          comment,
          comment_url,
          discard,
          domain,
          max_age,
          expires,
          path,
          port,
          secure,
          version}).

%% HTTP Protocol
-define(GET, "GET").
-define(POST, "POST").
-define(PUT, "PUT").
-define(HEAD, "HEAD").
-define(DELETE, "DELETE").
-define(OPTIONS, "OPTIONS").

-define(USER_AGENT, "Tsung").
-define(USER_AGENT_ERROR_MSG, "Total sum of user agents frequency is not equal to 100").


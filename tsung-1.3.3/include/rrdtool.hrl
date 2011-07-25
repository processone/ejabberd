%%%
%%%  Copyright © IDEALX S.A.S. 2004
%%%
%%%  Author : Nicolas Niclausse <nicolas.niclausse@niclux.org>
%%%  Created: 06 May 2004 by Nicolas Niclausse <nicolas.niclausse@niclux.org>
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

-vc('$Id$ ').
-author('nicolas.niclausse@niclux.org').

-record(rrd_create, {filename,
                     step=300, % one step every 5mn by def.
                     ds  =[],
                     rra =[] }).


% [RRA:CF:xff:steps:rows]
-record(rrd_rra, {cf   = "AVERAGE",  % MIN|MAX|AVERAGE|LAST
                  xff  = "0.1",
                  steps= 1,
                  rows = 288}).

% [DS:ds-name:DST:heartbeat:min:max]
-record(rrd_ds, {name,
                 type = "GAUGE",  % GAUGE|COUNTER|DERIVE|ABSOLUTE
                 heartbeat = 600, % 10mn
                 min = "U",
                 max = "U" }).

-record(rrd_update, {filename, updates=[], template}).

-record(rrd_supdate, {timestamp="N", values=[]}).

-record(rrd_graph, {filename,
                    starttime,
                    endtime,
                    height,
                    vlabel,
                    defs=[],
                    lines=[],
                    cdefs=[],
                    areas,
                    vnames,
                    hrules,
                    vrules,
                    print,  %#rrd_print
                    gprint, %#rrd_print
                    comment,% string
                    stack }).

% [DEF:vname=rrd:ds-name:CF]
-record(rrd_def, {vname,
                  filename,
                  ds_name,
                  cf ="AVERAGE"}).
% [CDEF:vname=rpn-expression]
-record(rrd_cdef, {vname,
                  rpnexpr }).

% [PRINT:vname:CF:format]
-record(rrd_print, {vname,
                    cf="AVERAGE",
                    format }).
% [GPRINT:vname:CF:format]
-record(rrd_gprint, {vname,
                    cf="AVERAGE",
                    format }).

% [HRULE:value#rrggbb[:legend]]
% [VRULE:time#rrggbb[:legend]]
-record(rrd_vrule, {value,
                    color,
                    legend }).
-record(rrd_hrule, {value,
                    color,
                    legend }).

% [LINE{1|2|3}:vname[#rrggbb[:legend]]]
-record(rrd_line, {number,
                   vname,
                   color,
                   legend }).

% [AREA:vname[#rrggbb[:legend]]]
-record(rrd_area, { vname,
                    color,
                    legend }).
% [STACK:vname[#rrggbb[:legend]]]
-record(rrd_stack, { vname,
                     color,
                     legend }).



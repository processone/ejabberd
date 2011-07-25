#! /usr/bin/python
# -*- coding: utf-8 -*-

#  Copyright: 2006 by Dalibo <dalibo.com>
#  Copyright: 2007 Dimitri Fontaine <dim@tapoueh.org>
#  Created: 2006 by Dimitri Fontaine <dim@tapoueh.org>
#
#  Modified: 2008 by Nicolas Niclausse
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
#
#  In addition, as a special exception, you have the permission to
#  link the code of this program with any library released under
#  the EPL license and distribute linked combinations including
#  the two.

"""
Python classes for tsung log data usage
 data: generic tsung data
  sample
  counter
  gauge

 log: tsung log file parser, using sample, gauge and counter classes

This package has its own configuration file where to associate tsung
stats names with tsung stat type.
"""

from ConfigParser import ConfigParser

# data are produced every 10 seconds, by default
# we read real used interval in log file (deduced from timestamps)
INTERVAL  = 10
SEPARATOR = "# stats: dump at "
PREFIX    = "stats: "

class data:
    """ Tsung logged data, to be specialized into sample and counter """
    def __init__(self):
        pass

    def get(self, name):
        """ get named statistic """
        if name not in self.__dict__.keys():
            return None

        # don't return any value when no measure have been taken
        # durung interval
        if self.count == 0:
            return None

        if name == 'count' :
            if self.interval > 0:
                return self.count / self.interval
            else:
                return 0

        return self.__dict__[name]

class sample(data):
    """ tsung sample stats """

    def __init__(self, interval = INTERVAL, values = [0, 0, 0, 0, 0, 0]):
        """ init log data values """
        self.interval = interval

        self.count  = float(values[0])
        self.mean   = float(values[1])
        self.stdvar = float(values[2])
        self.min    = float(values[3])
        self.max    = float(values[4])
        self.gmean  = float(values[5])

    def __repr__(self):
        """ human readable output """
        return "sample: %s %s %s %s %s" % (self.value, self.mean,
                                           self.stdvar, self.min, self.max, self.gmean)

class counter(data):
    """ tsung counter stats """

    def __init__(self, interval = INTERVAL, values = [0, 0]):
        """ init log data values """
        self.interval = 1

        self.count      = float(values[0])
        self.totalcount = float(values[1])

    def __repr__(self):
        """ human readable output """
        return "counter: %s %s" % (self.value, self.totalcount)

class gauge(data):
    """ tsung gauge stats """

    def __init__(self, interval = INTERVAL, values = [0, 0]):
        """ init log data values """
        self.interval = 1

        self.count      = float(values[0])
        self.max        = float(values[1])

    def __repr__(self):
        """ human readable output """
        return "gauge: %s %s" % (self.count, self.max)

class TsungLog:
    """ Tsung logged data parser and representation

    data is a {timestamp, data} hash, with data either sample or
    counter instance.
    """

    def __init__(self, config_filename, filename):
        """ constructor """
        self.conffile = config_filename
        self.filename = filename
        self.types    = {}
        self.data     = {}
        self.unknown  = []

        self.configure()
        self.parse()

    def configure(self):
        """ read configuration file to associates stat types to stat names """

        config = ConfigParser()
        config.read(self.conffile)

        for s in config.sections():
            for (name, type) in config.items(s):
                if type == 'sample':
                    self.types[name] = sample()
                if type == 'counter':
                    self.types[name] = counter()
                if type == 'gauge':
                    self.types[name] = gauge()

    def parse(self):
        """ read data from self.filename """
        first_ts   = 0
        current_ts = 0
        record_ts  = 0
        import re

        for line in file(self.filename):
            # chomp \n
            line = line[:-1]

            if line.find(SEPARATOR) == 0:
                measure_ts = int(line[len(SEPARATOR):].strip())

                # interval in seconds between two records. As tsung
                # generates a reference line, sensible values won't
                # have to divide by 0
                interval = measure_ts - current_ts

                # now we change current_ts
                current_ts = measure_ts

                if first_ts == 0:
                    first_ts  = current_ts

                record_ts = current_ts - first_ts

            elif line.find("stats: ") == 0:
                array  = [v.strip() for v in line.split(" ")]
                name   = array[1]
                values = array[2:]

                if self.types.has_key(name):
                    data = self.types[name].__class__(interval, values)

                    if not self.data.has_key(record_ts):
                        self.data[record_ts] = {}

                    self.data[record_ts][name] = data

                else:
                    is_re = False
                    x = re.compile("^[\w\d]+$")
                    for k in self.types.keys():
                        if not re.match(x, k):
                            y = re.compile(k)
                            if re.match(y, name):
                                data = self.types[k].__class__(interval, values)
                                if not self.data.has_key(record_ts):
                                    self.data[record_ts] = {}
                                self.data[record_ts][name] = data
                                is_re = True
                                break

                    if name not in self.unknown and not is_re:
                        print 'WARNING: tsung %s data is not configured' % name
                        self.unknown.append(name)

    def stat(self, name, stat):
        """ returns a {timestamp: date_type} dict for given named statistic """
        ret = {}
        for ts, stats in self.data.items():
            if stats.has_key(name) and stats[name].get(stat) is not None:
                ret[ts] = stats[name].get(stat)

        return ret


if __name__ == '__main__':
    # some unit testing
    import sys

    config   = "stats.conf"
    logfile  = sys.argv[1]

    tsunglog = TsungLog(config, logfile)

    from pprint import pprint
    pprint(tsunglog.stat('request', 'mean'))
    pprint(tsunglog.stat('users', 'max'))
    pprint(tsunglog.stat('finish_users_count', 'count'))
    pprint(tsunglog.stat('200', 'count'))


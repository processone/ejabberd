#! /usr/bin/python
'''
Created on 10.08.2011

@author: Aleksandra Lipiec
'''

import sys
import time
import ConfigParser
from pygooglechart import XYLineChart
from pygooglechart import Axis
from zabbix_api import ZabbixAPI


class DataRetriever():
    server = "http://127.0.1.1/zabbix"
    username = "admin"
    password = "zabbix" 

    hist_type = 3
    dens = 1
    
    time_format = "%d-%m-%Y %H:%M:%S"
    
    def __init__(self, item_key):
        self.zapi = ZabbixAPI(server=self.server, path="", log_level=3)
        self.zapi.login(self.username, self.password)
        
        self.item_key = item_key
   
    def set_config(self, config):
        self.hist_type = config.get("general", "hist_type")
        self.dens = int(config.get("general", "dens"))

    # Time format: "%d-%m-%Y %H:%M:%s"
    def get_data(self, str_time_from, str_time_to):
        
        time_from = int(time.mktime(time.strptime(str_time_from, self.time_format)))
        time_to = int(time.mktime(time.strptime(str_time_to, self.time_format)))
        
        print str_time_from, time_from
        print str_time_to, time_to
        
        hostid = self.zapi.host.get({"output":"extend", "filter": {"host":"localhost"}})[0]["hostid"]
        itemid = self.zapi.item.get({"output" : "extend",
                                     "hostids" : [hostid], 
                                     "filter" : {"key_" : self.item_key}})[0]['itemid']

        H = self.zapi.history.get({"time_from" : str(time_from), 
                                   "time_till" : str(time_to), 
                                   "output":"extend", 
                                   "itemids" : [itemid],
                                   "hostids" : [hostid],
                                   "history" : self.hist_type})
        result = [[], []]
        i = 0
        for el in H:
            i += 1
            if i % self.dens == 0:
                result[0].append(int(el["clock"]) - time_from)
                result[1].append(float(el["value"]))
        
        return result
  
class Graph():
    
    colours = []
    legend = []
    max_wx = 0
    max_wy = 0
    min_wy = 4000000000
    
    label_suffix = " MB"
    scale = 1024*1024

    def __init__(self):
        self.chart = XYLineChart(750, 400)
        
    
    def set_config(self, config):
        self.label_suffix = config.get("general", "label_suffix")
        self.scale = int(config.get("general", "scale"))

    def add_serie(self, WX, WY, colour, descr):
        self.chart.add_data(WX)
        self.chart.add_data(WY)
        self.colours.append(colour)
        self.legend.append(descr)
        
        self.max_wx = max(self.max_wx, max(WX))
        self.max_wy = max(self.max_wy, max(WY))
        self.min_wy = min(self.min_wy, min(WY))
        
    def set_title(self, title):
        self.chart.set_title(title)
        
    def make_chart(self, name):
        densX = self.max_wx / 15
        
        wy_max_lab = int(self.max_wy / self.scale + 1)
        wy_min_lab = int(max(self.min_wy / self.scale - 1, 0))
        densY = max((wy_max_lab - wy_min_lab) / 10, 1) 
        self.chart.set_axis_labels(Axis.BOTTOM, range(0, self.max_wx + 1, densX))
        self.chart.set_axis_labels(Axis.LEFT, self.__make_Ylabels(wy_min_lab, wy_max_lab, int(densY)))
        self.chart.y_range = (wy_min_lab * self.scale, wy_max_lab * self.scale)
        
        self.chart.set_legend(self.legend)
        self.chart.set_colours(self.colours)
        self.chart.set_grid(15, 10, 1, 5)
        
        self.chart.download("%s.png" % (name))
        
    def __make_Ylabels(self, wy_min_lab, wy_max_lab, densY):
        result = []
        for i in range(wy_min_lab, wy_max_lab, densY):
            result.append("%d%s" % (i, self.label_suffix))
        return result

def main():
    
    config = ConfigParser.ConfigParser()
    config.read('graphs.cfg')
    
    series_num = int(config.get("general", "series_num"))
    title = config.get("general", "title")
    filename = config.get("general", "filename")
    
    graph = Graph()
    graph.set_config(config)

    wyX = []
    
    for i in range(1, series_num + 1):
        # retrieve data serie
        section = "serie" + str(i)
        
        item_key = config.get(section, "key")
        time_from = config.get(section, "time_from")
        time_to = config.get(section, "time_to")
        colour = config.get(section, "colour")
        legend = config.get(section, "legend")
        
        retriever = DataRetriever(item_key)
        retriever.set_config(config)
        data = retriever.get_data(time_from, time_to)
        for i in range(0, len(data[0])):
            print ("%d : %f" % (data[0][i], data[1][i]))

        if len(wyX) == 0:
            wyX = data[0]
            
        graph.add_serie(wyX, data[1], colour, legend)
            
    #create graph
    graph.set_title(title)
    graph.make_chart(filename)
    

if __name__ == "__main__":
    main()
            



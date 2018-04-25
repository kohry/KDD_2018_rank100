# -*- coding: utf-8 -*-

import os
import sys
import urllib.request
import json
import csv
import time
import pandas
import datetime
import traceback
import random
from io import StringIO
import time


def get_info(urlgo):

    ##  <REPLACE this key with your own api key. openweathermap.org > ## 

    
    request = urllib.request.Request(urlgo)
    response = urllib.request.urlopen(request)
    rescode = response.getcode()

    outputDf = pandas.DataFrame()

    if(rescode==200):
        response_body = response.read()
        raw = response_body.decode('utf-8')
        try :
           outputDf = pandas.read_csv(StringIO(raw), sep = ",")
           print(outputDf)

        except Exception as e: 
            print(e)
            exc_type, exc_obj, exc_tb = sys.exc_info()
            fname = os.path.split(exc_tb.tb_frame.f_code.co_filename)[1]
            print(exc_type, fname, exc_tb.tb_lineno)
        
        finally : 
            return outputDf

    else:
        print("Error Code:" + rescode)
        return outputDf

# for write df to csv
def write_csv_from_df(df, filename):
    df.to_csv(filename, sep = ',')

print("---------start---------------")

out = pandas.DataFrame()

merge = pandas.DataFrame()

try :
    out1 = get_info("https://biendata.com/competition/airquality/ld/2018-04-23-0/2018-05-31-23/2k0d1d8")
    out2 = get_info("https://biendata.com/competition/airquality/bj/2018-04-23-0/2018-05-31-23/2k0d1d8")

    merge = pandas.concat([out1, out2])
    
except Exception as e: 
    print(e)
    exc_type, exc_obj, exc_tb = sys.exc_info()
    fname = os.path.split(exc_tb.tb_frame.f_code.co_filename)[1]
    print(exc_type, fname, exc_tb.tb_lineno)

finally : 
    write_csv_from_df(merge,"weather_pollution.csv")
    write_csv_from_df(merge,"weather_pollution" + str(time.time()) + ".csv")
    print("-----------end-------------")

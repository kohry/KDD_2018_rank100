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

class Location :
    lat = 0
    lon = 0

class Info :
    date = 1523977200
    temp = '0'
    pressure = 0
    humidity = 0
    weather = 'what'
    weather_code = '999'
    weather_desc = 'desc'
    weather_cloud = 999
    weather_rain = 999
    wind_speed = 0
    wind_direction = 360

# 2018.04.08 - Read city list from csv
def read_file():
    with open('bulk.json') as json_data:
        return json.load(json_data)
    
def get_info(raw):

    outputDf = pandas.DataFrame()

    ii = 0

    try :
        while True:

            ii = ii + 1

            # get temperature from returned json list
            # <REPLACE node with this if needed>
            stationId = raw[ii]['city_id']
            date = datetime.datetime.fromtimestamp(int(raw[ii]['dt'])).strftime('%Y-%m-%d %H:%M:%S')
            temperature = raw[ii]['main'] ['temp'] - 273.15
            pressure = raw[ii]['main'] ['pressure']
            humidity = raw[ii]['main'] ['humidity']
            weather = raw[ii]['weather'][0]['main']
            weather_code = raw[ii]['weather'][0]['id']
            weather_desc = raw[ii]['weather'][0]['description']
            weather_cloud = raw[ii]['clouds']['all']
            weather_rain = 0
            try :
                weather_rain = raw[ii]['rain']['3h']
            except:
                weather_rain = 0

            wind_speed = raw[ii]['wind']['speed']

            wind_direction = 0
            try :
                wind_direction = raw[ii]['wind']['deg']
            except:
                wind_direction = round(random.random()*360)
            
            df = pandas.DataFrame([[stationId, date, temperature, pressure, humidity, weather, weather_code, weather_desc, weather_cloud, weather_rain, wind_speed, wind_direction]], columns=['stationId','date','temperature','pressure','humidity','weather','weather_code','weather_desc','weather_cloud','weather_rain','wind_speed','wind_direction'])
            print(df)   

            outputDf = outputDf.append(df)

    except Exception as e: 
        print(e)
        exc_type, exc_obj, exc_tb = sys.exc_info()
        fname = os.path.split(exc_tb.tb_frame.f_code.co_filename)[1]
        print(exc_type, fname, exc_tb.tb_lineno)
    
    finally : 
        return outputDf



# for write df to csv
def write_csv_from_df(df, filename):
    df.to_csv(filename, sep = ',')

print("---------start---------------")

# read city list from file. you can just hardcode the city list in this python file.
raw = read_file() 
out = pandas.DataFrame()

try :
    out = get_info(raw)

except :
    print("error")

finally : 
    write_csv_from_df(out, "./bulk.csv")
    print("-----------end-------------")

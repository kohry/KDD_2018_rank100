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
    
    city_id = 0
    city_name = 's'
    city_coord_lat = 0.1
    city_coord_long = 0.1

# 2018.04.08 - Read city list from csv
def read_file():
    dic = dict()

    ## <REPLACE XXXX with your city list> ##
    f = open('station.csv', 'r', encoding='utf-8', newline='') 
    rdr = csv.reader(f)
    for line in rdr:
        station = line[0]
        location = Location()
        location.lat = line[1]
        location.lon = line[2]
        dic[station] = location
    f.close()    
    return dic

def get_info(station, lat, lon):

    encText = urllib.parse.quote(station.encode('utf-8'))

    ##  <REPLACE this key with your own api key. openweathermap.org > ## 
    apiKey = "22d4ef82c7c5db24d5cbbf52088974f5" 
    url = "http://api.openweathermap.org/data/2.5/forecast?lat=" + lat + "&lon=" + lon + "&appid=" + apiKey 
    
    request = urllib.request.Request(url)
    response = urllib.request.urlopen(request)
    rescode = response.getcode()

    info = Info()
    outputDf = pandas.DataFrame()

    if(rescode==200):
        response_body = response.read()
        result = response_body.decode('utf-8')
        raw = json.loads(result)
        try :

            for ii in range(1) :
                
                city_id = raw['city']['id']
                city_name = raw['city']['name']
                city_coord_lat = raw['city']['coord']['lat']
                city_coord_long = raw['city']['coord']['lon']

                df = pandas.DataFrame([[station, city_id, city_name, city_coord_lat, city_coord_long]], columns=['station','city_id', 'city_name', 'city_coord_lat', 'city_coord_long'])
                print(df)   

                outputDf = outputDf.append(df)

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

# for write out dict object
def write_file(dic, file_name):
    with open(file_name, 'w') as f:
        fieldnames = ['stationId','temperature','pressure','humidity','weather','weather_code','weather_desc','weather_cloud','weather_rain','wind_speed','wind_direction']
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        data = [dict(zip(fieldnames, [k, v])) for k, v in dic.items()]
        writer.writerows(data)

# for write df to csv
def write_csv_from_df(df, filename):
    df.to_csv(filename, sep = ',')

print("---------start---------------")

# read city list from file. you can just hardcode the city list in this python file.
dic = read_file() 

out = pandas.DataFrame()

try :
    # put score into the dictionary dic, if exists.
    for city in dic:
        location = dic[city]
        print(city)
        out = out.append(get_info(city, location.lat, location.lon)) #unit
        time.sleep(1) # API will possibly block your request without this. replace it if you want.

except :
    print("error")

finally : 
    write_csv_from_df(out, "./weather_city.csv")
    print("-----------end-------------")

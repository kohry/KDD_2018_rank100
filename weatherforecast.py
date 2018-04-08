# -*- coding: utf-8 -*-

import os
import sys
import urllib.request
import json
import csv
import time

# 2018.04.08 - Read city list from csv
def read_file():
    dic = dict()

    ## <REPLACE XXXX with your city list> ##
    f = open('XXXX.csv', 'r', encoding='utf-8', newline='') 
    rdr = csv.reader(f)
    for line in rdr:
        zipcode = line[0]
        dic[zipcode]=0 
    f.close()    
    return dic

def get_temp(cityzipcode):

    encText = urllib.parse.quote(city.encode('utf-8'))
    countrycode = "cn"

    ##  <REPLACE this key with your own api key. openweathermap.org > ## 
    apiKey = "" 
    url = "http://api.openweathermap.org/data/2.5/forecast?zip=" + cityzipcode + "," + countrycode + "&appid=" + apiKey 
    
    request = urllib.request.Request(url)
    response = urllib.request.urlopen(request)
    rescode = response.getcode()

    if(rescode==200):
        response_body = response.read()
        result = response_body.decode('utf-8')
        raw = json.loads(result)
        try :
            # get temperature from returned json list
            # <REPLACE node with this if needed>
            temp = raw['list'][0]['temp'] 
        except :
            temp = 0 
        return temp

    else:
        print("Error Code:" + rescode)
        return 0

def write_file(dic, file_name):
    with open(file_name, 'w') as f:
        fieldnames = ['City', 'Temp']
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        data = [dict(zip(fieldnames, [k, v])) for k, v in dic.items()]
        writer.writerows(data)

print("---------start---------------")


# read city list from file. you can just hardcode the city list in this python file.
dic = read_file() 

try :
    # put score into the dictionary dic, if exists.
    for city in dic:
        temp = get_temp(city)
        print(f"{city} - {temp}")
        dic[city] = temp
        time.sleep(1) # API will possibly block your request without this. replace it if you want.

finally : # sometimes, 500 error occurs.
    # when all done, save it to csv
    write_file(dic, "temp.csv")
    

print("-----------end-------------")
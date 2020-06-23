#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#configure_mimic.py

#----- Import libraries -----#
import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import psycopg2
import datetime
from IPython.display import display, HTML # used to print out pretty pandas dataframes
import matplotlib.dates as dates
import matplotlib.lines as mlines
from sqlalchemy import create_engine
from sqlalchemy_utils import database_exists, create_database

#----- Set up mimic database -----#
#(see https://mimic.physionet.org/gettingstarted/dbsetup/ for instuctions on building this)

# specify user/password/where for MIMIC
sqluser = 'postgres'
sqlpass = 'postgres'
dbname = 'mimic'
schema_name = 'mimiciii'
host = 'localhost'

query_schema = 'SET search_path to ' + schema_name + ';'

# connect to the database
con = psycopg2.connect(dbname=dbname, user=sqluser, password=sqlpass, host=host)
print('\nConfigured for local MIMIC database:')
print('\tuser = {}'.format(sqluser))
print('\tdbname = {}'.format(dbname))
print('\thost = {}'.format(host))
print('\tschema_name = {}'.format(schema_name))
print('\tquery_schema = {}'.format(query_schema))


#----- Set up db for modified mimic tables -----#
#local directory to keep files in
my_local_data_dir = '../../../my_mimic/'
import subprocess
process = subprocess.Popen(['mkdir', my_local_data_dir],
                     stdout=subprocess.PIPE, 
                     stderr=subprocess.PIPE)
stdout, stderr = process.communicate()

#postgreSQL vars
my_dbname = 'my_mimic_db'
my_username = 'postgres'
my_pswd = 'my_mimic'
my_schema_name = 'my_mimic_db'
my_schema = 'SET search_path to ' + my_schema_name + ';'



## 'engine' is a connection to a database
## Here, we're using postgres, but sqlalchemy can connect to other things too.
engine = create_engine('postgresql://%s:%s@localhost/%s'%(my_username,my_pswd,my_dbname))
my_con = psycopg2.connect(database = my_dbname, user = my_username, host='localhost', password=my_pswd)
#print('postgresql://%s:%s@localhost/%s'%(username,pswd,dbname))
#print(engine.url)

## create a database (if it doesn't exist)
if not database_exists(engine.url):
    create_database(engine.url)
#print(database_exists(engine.url))
#print(engine.url)

print('\nConfigured for my local MIMIC database (modified tables):')
print('\tmy_username = {}'.format(sqluser))
print('\tmy_dbname = {}'.format(dbname))
print('\thost = localhost')
print('\tmy_schema_name = {}'.format(my_schema_name))
print('\tmy_schema = {}'.format(my_schema))



#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#configure_mimic.py

# Import libraries
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import psycopg2
import datetime
from IPython.display import display, HTML # used to print out pretty pandas dataframes
import matplotlib.dates as dates
import matplotlib.lines as mlines

#%matplotlib inline
#plt.style.use('ggplot')

# specify user/password/where for MIMIC
sqluser = 'postgres'
sqlpass = 'postgres'
dbname = 'mimic'
schema_name = 'mimiciii'
host = 'localhost'

query_schema = 'SET search_path to ' + schema_name + ';'

# connect to the database
con = psycopg2.connect(dbname=dbname, user=sqluser, password=sqlpass, host=host)
print('\nConfigured for local MIMIC database.')
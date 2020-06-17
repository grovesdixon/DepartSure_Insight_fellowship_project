#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jun 17 11:31:04 2020

@author: grovesdixon
"""


#configure for local mimic databse
exec(open("../configs/configure_mimic.py").read())
exec(open("my_functions.py").read())

adat = pd.read_csv('/Users/grovesdixon/projects/Insight/icu_project/my_mimic/outcome_penalizable_dat.csv')
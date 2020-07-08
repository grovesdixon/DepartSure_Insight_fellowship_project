# DepartSure
An application to predict and prevent early hospital readmissions

## Motivation
Under the Hospital Readmissions Reduction Program, hospitals can 
be penalized for excess readmissions within 30 days of discharge. 
Departsure uses predictive modeling to flag pateints at high risk
of readmission during the dischage process


## Project summary
This project is part of a Health Data fellowship with Insight Data Science. 
It is developed based on the MIMIC-III database (https://mimic.physionet.org/),
and requires the user to gain permission to download the database and build a local
PostgreSQL database following their instructions (https://mimic.physionet.org/gettingstarted/access/).
This repo contains code to: 1) format the outcome variable, 2)select features, 3)predict the outcome,
4) output files for the associated web application (https://github.com/grovesdixon/insight_webapp).
Scripts to perform these steps are in the project_framework/ directory.

### Format outcome variable
build_outcome.ipynb
Query a local MIMIC-III database and format the outcome variable (readmission within 30 days of discharge)

### Select features
MIMIC has thousands of potential predictive features. Use feature_engineering.ipynb selects features based on occurance among records and random forest feature importance.
MIMIC also includes unstructured discharge notes. Use discharge_notes.ipynb to extract features from these using a bag of words approach.

### Predict outcome
Use predict.ipynb to apply logistic regression and random forest to predict outcome varible and test performance.

## data
Final data files for plotting figures for slides and the web application are saved here. Plot figures with for_plotting/build_plots.R

## Configure
Conifguration of the local PostgreSQL database and locations for output files is given in configs/configure_mimic.py.

## Project slides
https://docs.google.com/presentation/d/1KVu20HxXc6pJsxCHaPX81zOntzvs2YkXWCSB6Vf4ISM/edit?usp=sharing

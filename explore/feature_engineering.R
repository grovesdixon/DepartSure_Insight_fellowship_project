#feature_engineering_mimic.R
rm(list=ls())
library(tidyverse)
library(fastDummies)
source('explore/explore.R')
# input = '~/projects/Insight/icu_project/my_mimic/readmission_cdiff.csv'; outname='~/projects/Insight/icu_project/my_mimic/readmission_cdiff_features.csv'; MIN_COUNT=5
# input = '~/projects/Insight/icu_project/my_mimic/readmission_quickCdiff.csv'; outname='~/projects/Insight/icu_project/my_mimic/readmission_quickCdiff_features.csv'
# input = '~/projects/Insight/icu_project/my_mimic/readmission_after_cdiff.csv';outname='~/projects/Insight/icu_project/my_mimic/readmission_after_cdiff_features.csv'
input = '~/projects/Insight/icu_project/my_mimic/any_readmission.csv'; outname='~/projects/Insight/icu_project/my_mimic/any_readmission_features.csv';MIN_COUNT=150

dat = read_csv(input)
sums = dat %>% 
  filter(!duplicated(HADM_ID))
sum(is.na(sums$RELIGION))
sum(sums$outcome)

get_cor = function(dat, xcol, ycol){
  x=dat[,xcol]
  y=dat[,ycol]
  xy = na.omit(data.frame('x'=x,'y'=y))
  colnames(xy) = c('x','y')
  cor(xy$x, xy$y)
}

# patient features --------------------------------------------------------

pdat = read_csv(paste(mimic_path, 'PATIENTS.csv', sep=''))
dat_p = dat %>% 
  left_join(pdat, by = 'SUBJECT_ID')


#age
dat_p$age = as.numeric(difftime(dat_p$ADMITTIME, dat_p$DOB,units = c('days')))/365
dat_p = dat_p %>% 
  filter(age > 18) %>% 
  mutate(age = if_else(age > 200,
                       91,
                       age))
get_cor(dat_p, 'age', 'outcome')
dat_p %>% 
  ggplot(aes(fill=factor(outcome), x=age)) +
  geom_density(alpha=0.5)


#gender
dat_p %>% 
  group_by(GENDER) %>% 
  summarize(rate = sum(outcome)/n())

#select patient features
patient_features = dat_p %>% 
  select(SUBJECT_ID,
         age,
         GENDER) %>% 
  group_by(SUBJECT_ID, GENDER) %>% 
  summarize(age=mean(age))

# admission features ------------------------------------------------------

#insurance
dat %>% 
  group_by(INSURANCE) %>% 
  summarize(positive_rate = sum(outcome)/n()) %>% 
  arrange(desc(positive_rate)) %>% 
  mutate(insurance = factor(INSURANCE, levels = INSURANCE)) %>% 
  ggplot(aes(x=insurance, y=positive_rate)) +
  scale_y_continuous(expand = c(0, 0)) +
  # scale_x_discrete(expand = c(0, 0)) +
  geom_bar(stat='identity') +
  labs(y='readmission rate',
      x='insurance type')

#regigion
dat %>% 
  group_by(RELIGION) %>% 
  summarize(N=n(),
            pos = sum(outcome),
            positive_rate = sum(outcome)/n()) %>% 
  data.frame()

#marital status
dat %>% 
  group_by(MARITAL_STATUS) %>% 
  summarize(N=n(),
            pos = sum(outcome),
            positive_rate = sum(outcome)/n()) %>% 
  data.frame()

#hospital duration
dat$hospital_duration = as.numeric(difftime(dat$DISCHTIME, dat$ADMITTIME,units = c('days')))
dat %>% 
  ggplot(aes(x=hospital_duration)) +
  geom_histogram()
  

#begin the selected features dataframe
selected_features = dat %>% 
  select(outcome,
         SUBJECT_ID,
         HADM_ID,
         INSURANCE,
         RELIGION,
         MARITAL_STATUS,
         hospital_duration,
         ADMISSION_LOCATION,
         DISCHARGE_LOCATION) %>% 
  left_join(patient_features)
feature_list = list()
feature_list[['patient']] = selected_features


# add ICU features --------------------------------------------------------

#gather features
idat = read_csv(paste(mimic_path, 'ICUSTAYS.csv', sep=''))
idat$icu_days = as.numeric(difftime(idat$OUTTIME, idat$INTIME), units="days")
ifeatures = idat %>%
  group_by(HADM_ID) %>%
  summarize(nicu_stays = n(),
            total_icu_days = sum(icu_days))

#merge with dataset
dat_i = dat %>%
  left_join(ifeatures, by = 'HADM_ID')


#EXPLORE
get_cor(dat_i, 'nicu_stays', 'outcome')
dat_i %>%
  ggplot(aes(x=nicu_stays)) +
  geom_histogram()

get_cor(dat_i, 'total_icu_days', 'outcome')
dat_i %>%
  ggplot(aes(x=total_icu_days)) +
  geom_histogram()


#select icu data features
icu_features = dat_i %>% 
  select(SUBJECT_ID, HADM_ID, nicu_stays, total_icu_days)

#add to feature list
sum(dat$HADM_ID %in% icu_features$HADM_ID)
feature_list[['icu']] = icu_features
names(feature_list)


# add diagnoses -----------------------------------------------------------

ddat = read_csv(paste(mimic_path, 'DIAGNOSES_ICD.csv', sep='')) %>% 
  select(SUBJECT_ID,
         HADM_ID,
         ICD9_CODE)

dat_d = dat %>% 
  left_join(ddat, by = c("SUBJECT_ID", "HADM_ID")) %>% 
  select(c(colnames(ddat), 'outcome'))


#get category correlations
get_category_outcome_cors = function(df, feature_column, min_count = 5){
  pos_df = df %>% 
    filter(outcome==1) %>% 
    data.frame()
  fcount = table(pos_df[,feature_column])
  enough = names(fcount)[fcount>min_count]
  print(paste(length(enough), 'of', length(fcount), 'features being checked...'))
  df_features = data.frame(df)[,feature_column]
  input_df = df[df_features %in% enough, ] %>% 
    select(c(feature_column, 'outcome'))
  dummy_dat = dummy_cols(input_df, select_columns = c(feature_column)) %>% 
    select(contains(feature_column), 'outcome') %>% 
    select(-feature_column)
  cdf = cor(dummy_dat) %>% 
    data.frame() %>% 
    rownames_to_column(var = 'var1') %>%
    pivot_longer(-var1,
                 names_to = 'var2',
                 values_to = 'cor') %>% 
    filter(cor < 1,
           var1=='outcome')
  return(cdf)
}

get_top_features = function(cdf, n_feature){
  top = cdf %>% 
    arrange(desc(abs(cor))) %>% 
    data.frame()
  top[1:n_feature,] %>% 
    as_tibble()
}

diag_cdf = get_category_outcome_cors(dat_d, 'ICD9_CODE', min_count = MIN_COUNT)
diag_cdf %>% 
  ggplot(aes(x=cor)) +
  geom_histogram()
dim(diag_cdf)
top_diag = get_top_features(diag_cdf, 20)
top_features = sub('ICD9_CODE_', '', top_diag$var2)
top_features = append(top_features, '00845')

#select diagnoses
sel_diagnoses = dat_d %>% 
  filter(ICD9_CODE %in% top_features) %>% 
  rename(diagnosis_icd9 = ICD9_CODE) %>% 
  select(SUBJECT_ID, HADM_ID, diagnosis_icd9) 


#total diagnoses
tdiag = dat_d %>% 
  group_by(SUBJECT_ID, HADM_ID) %>% 
  summarize(total_diagnoses = length(unique(ICD9_CODE)))

#select daignosis features
diagnosis_features = tdiag %>% 
  left_join(sel_diagnoses)

#add to feature list
sum(dat$HADM_ID %in% tdiag$HADM_ID)
sum(dat$HADM_ID %in% diagnosis_features$HADM_ID)
feature_list[['diagnosis']] = diagnosis_features
names(feature_list)

#look at the important diagnoses
dd_dat = read_csv(paste(mimic_path, 'D_ICD_DIAGNOSES.csv', sep='')) %>% 
  filter(ICD9_CODE %in% sel_diagnoses$diagnosis_icd9)
infection_codes = dd_dat %>% 
  filter(grepl('^Infection', LONG_TITLE))

# add procedure features ------------------------------------------------------

#upload procedures data
proc_dat = read_csv(paste(mimic_path, 'PROCEDURES_ICD.csv', sep=''))
length(unique(proc_dat$ICD9_CODE))

#merge with otucomes
dat_proc = dat %>% 
  select(c("SUBJECT_ID", "HADM_ID", "outcome")) %>% 
  left_join(proc_dat, by = c("SUBJECT_ID", "HADM_ID")) %>% 
  select(c(colnames(proc_dat), 'outcome'))

#select relevent features
proc_cdf = get_category_outcome_cors(dat_proc, 'ICD9_CODE', min_count = MIN_COUNT)
proc_cdf %>% 
  ggplot(aes(x=cor)) +
  geom_histogram()
dim(proc_cdf)
top_proc = get_top_features(diag_cdf, 20)
top_features = sub('ICD9_CODE_', '', top_proc$var2)

#select procedures
sel_procedures = dat_proc %>% 
  filter(ICD9_CODE %in% top_features) %>% 
  rename(procedure_icd9 = ICD9_CODE) %>% 
  select(SUBJECT_ID, HADM_ID, procedure_icd9)

#get total procedures
tprocedure = dat_proc %>% 
  group_by(SUBJECT_ID, HADM_ID) %>% 
  summarize(total_procedure = length(unique(ICD9_CODE)))

#select daignosis features
procedure_features = tprocedure %>% 
  left_join(sel_procedures)

#add to feature list
sum(dat$HADM_ID %in% tprocedure$HADM_ID)
sum(dat$HADM_ID %in% procedure_features$HADM_ID)
feature_list[['procedure']] = procedure_features

# add prescription features -----------------------------------------------

#upload prescription data
pre_dat = read_csv(paste(mimic_path, 'PRESCRIPTIONS.csv', sep=''))
unique(pre_dat$DRUG_TYPE)

#merge with outcomes
dat_pre = dat %>% 
  select(c("SUBJECT_ID", "HADM_ID", 'outcome')) %>% 
  left_join(pre_dat, by = c("SUBJECT_ID", "HADM_ID")) %>% 
  select(c(colnames(pre_dat), 'outcome'))

#select important drug features
drug_cdf = get_category_outcome_cors(dat_pre, 'DRUG', min_count = MIN_COUNT)
drug_cdf %>% 
  ggplot(aes(x=cor)) +
  geom_histogram()
dim(drug_cdf)
top_proc = get_top_features(drug_cdf, 20)
top_drugs = sub('DRUG_', '', top_proc$var2)

#select procedures
sel_drugs = dat_pre %>% 
  filter(DRUG %in% top_drugs) %>% 
  select(SUBJECT_ID, HADM_ID, DRUG)

#get total durgs
tdrugs = dat_pre %>% 
  group_by(HADM_ID) %>% 
  summarize(total_drug = length(unique(DRUG)))

#select daignosis features
prescription_features = tdrugs %>% 
  left_join(sel_drugs)

#add to feature list
sum(dat$HADM_ID %in% prescription_features$HADM_ID)
feature_list[['prescription']] = prescription_features


# assemble features -------------------------------------------------------

names(feature_list)
feature_dat = purrr::reduce(feature_list, left_join, by=c('SUBJECT_ID','HADM_ID'))
sum(is.na(feature_dat$total_procedure))
length(unique(feature_dat$HADM_ID))
feature_dat %>% 
  write_csv(outname)


# output readmission diagnosis prediction set -----------------------------

#merege with returns
ll=load('~/projects/Insight/icu_project/my_mimic/quick_return_df.Rdata')
ll
quick_return_df
quick_toadd = quick_return_df %>% 
  select(HADM_ID, readmission_hadm_id)

#isolate readmissions
read_features = feature_dat %>% 
  filter(outcome==1) %>% 
  left_join(quick_toadd, by = 'HADM_ID')


#create new outcome varible for infection in readmission
infection_codes
dim(read_features)
colnames(read_features)

#reload the diagnoses
ddat = read_csv(paste(mimic_path, 'DIAGNOSES_ICD.csv', sep='')) %>% 
  select(SUBJECT_ID,
         HADM_ID,
         ICD9_CODE,
         ROW_ID)



  filter(LONG_TITLE %in% infection_codes$LONG_TITLE) %>% 
  rename(HADM_ID = readmission_hadm_id) %>% 
  rename(LONG_TITLE = readmission)


# explore features --------------------------------------------------------

#insurance
dat %>% 
  group_by(INSURANCE) %>% 
  summarize(positive_rate = sum(outcome)/n()) %>% 
  arrange(desc(positive_rate)) %>% 
  mutate(insurance = factor(INSURANCE, levels = INSURANCE)) %>% 
  ggplot(aes(x=insurance, y=positive_rate)) +
  scale_y_continuous(expand = c(0, 0)) +
  # scale_x_discrete(expand = c(0, 0)) +
  geom_bar(stat='identity') +
  labs(y='readmission rate',
       x='insurance type')

#age
feature_dat %>% 
  mutate(age2 = cut(feature_dat$age, breaks = seq(20, 100, by = 10))) %>% 
  group_by(age2) %>% 
  summarize(rate = sum(outcome)/n()) %>% 
  ggplot(aes(x=age2, y=rate)) +
  geom_point(size=5, color='grey25') +
  geom_smooth() +
  scale_x_discrete(labels = seq(20,100, by=10)) +
  labs(x='age',
       y='readmission rate')


#diagnosis
feature_dat %>% 
  group_by(outcome) %>% 
  summarize(rate = sum(diagnosis_icd9=='00845')/n()) %>% 
  ggplot(aes(x=age2, y=rate)) +
  geom_point(size=5, color='grey25') +
  geom_smooth() +
  scale_x_discrete(labels = seq(20,100, by=10)) +
  labs(x='age',
       y='readmission rate')

sum(feature_dat$diagnosis_icd9=='00845', na.rm=TRUE)
head(feature_dat) %>% 
  view()


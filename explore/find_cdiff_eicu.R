#find_cdiff_mimic.R
source('explore/explore.R')

#find out where cdiff information would live
#this could be in:
#diagnoses_icd (see d_icd_diagnoses for names)
  #Intestinal infection due to Clostridium difficile: ICD9_CODE = 00845
#d_microbiologyevents
  #ORG_NAME = CLOSTRIDIUM DIFFICILE


#load data
pdat = load_csv(eicu_path, 'patient.csv.gz')
ddat0 = load_csv(eicu_path, 'diagnosis.csv.gz')
ddat = ddat0 %>% 
  separate(icd9code, into = c('icd9code', 'icd9comma'), sep=',')
code_match = ddat$icd9code=='008.45'
str_match = grepl('difficil', ddat$diagnosisstring)
sum(code_match & !str_match, na.rm=TRUE)
sum(is.na(code_match) & str_match, na.rm=TRUE)
sum(str_match) - sum(code_match, na.rm=TRUE)


difficil = ddat %>% 
  filter(grepl('difficil', diagnosisstring))
view(difficil)


adat = read_csv(paste(mimic_path, 'ADMISSIONS.csv', sep=''))
ddat = read_csv(paste(mimic_path, 'DIAGNOSES_ICD.csv', sep='')) %>% 
  select(SUBJECT_ID,
         HADM_ID,
         ICD9_CODE)
mdat = read_csv(paste(mimic_path, 'MICROBIOLOGYEVENTS.csv', sep='')) %>% 
  select(SUBJECT_ID,
         HADM_ID,
         SPEC_TYPE_DESC,
         ORG_NAME)
dat_list = list(adat, ddat, mdat)
cdat = purrr::reduce(dat_list, left_join)
dim(cdat)

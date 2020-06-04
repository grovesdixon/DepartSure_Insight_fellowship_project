#merge_eicu_and_mimic.R
source('explore/explore.R')

# load mimic data ---------------------------------------------------------
mp_dat = read_csv(paste(mimic_path, 'PATIENTS.csv', sep='')) %>% 
  select(SUBJECT_ID,
         GENDER,
         DOB)
ma_dat = read_csv(paste(mimic_path, 'ADMISSIONS.csv', sep='')) %>% 
  select(SUBJECT_ID,
         HADM_ID,
         ADMITTIME,
         ADMISSION_TYPE,
         ETHNICITY,
         HAS_CHARTEVENTS_DATA) %>% 
  filter(HAS_CHARTEVENTS_DATA==1)
md_dat = read_csv(paste(mimic_path, 'DIAGNOSES_ICD.csv', sep='')) %>% 
  select(SUBJECT_ID,
         HADM_ID,
         ICD9_CODE) %>% 
  rename(diagnosis_icd9 = ICD9_CODE)
mm_dat = read_csv(paste(mimic_path, 'MICROBIOLOGYEVENTS.csv', sep='')) %>% 
  select(SUBJECT_ID,
         HADM_ID,
         SPEC_TYPE_DESC,
         ORG_NAME)
mproc_dat = read_csv(paste(mimic_path, 'PROCEDURES_ICD.csv', sep='')) %>% 
  select(SUBJECT_ID,
         HADM_ID,
         ICD9_CODE) %>% 
  rename(procedure_icd9 = ICD9_CODE)

dat_list = list(mp_dat,
                ma_dat,
                md_dat,
                mm_dat,
                mproc_dat)
mdat = purrr::reduce(dat_list, left_join)
dim(mdat)
head(mdat)


patient_list = list()

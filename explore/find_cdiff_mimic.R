#find_cdiff_mimic.R
rm(list=ls())
source('explore/explore.R')

#find out where cdiff information would live
#this could be in:
#diagnoses_icd (see d_icd_diagnoses for names)
  #Intestinal infection due to Clostridium difficile: ICD9_CODE = 00845
#d_microbiologyevents
  #ORG_NAME = CLOSTRIDIUM DIFFICILE


#load data
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

#merge on HADM_ID
#this is the unique admission id for a given pateint
#a single individual with unique SUBJECT_ID can have
#multiple HADM_IDs, indicating multiple admissions
dat_list = list(adat, ddat, mdat)
cdat = purrr::reduce(dat_list, left_join)
head(cdat)
dim(cdat)


# look at patient return --------------------------------------------------

#total patients
length(unique(cdat$SUBJECT_ID))

#total return visits
vdat = cdat %>% 
  group_by(SUBJECT_ID) %>% 
  summarize(tvisists = length(unique(HADM_ID)))
vdat %>% 
  ggplot(aes(x=tvisists)) +
  geom_bar() +
  lims(x=c(0,6))
sum(vdat$tvisists > 1)
return_ids = vdat %>% 
  filter(tvisists > 1) %>% 
  pull(SUBJECT_ID)

#total return visits within 30 days
rdat = cdat %>% 
  filter(SUBJECT_ID %in% return_ids) %>% 
  select(SUBJECT_ID,
         HADM_ID,
         ADMITTIME) %>% 
  filter(!duplicated(HADM_ID))
dim(rdat)


check_returns = function(sid){
  sub = rdat %>% 
    filter(SUBJECT_ID==sid) %>% 
    arrange(ADMITTIME)
  diffs = diff(sub$ADMITTIME)
  diff_days = diffs
  units(diff_days)<-'days'
  res = min(diff_days) < 30
}

subjects = unique(rdat$SUBJECT_ID)
readmit = sapply(subjects, function(x) check_returns(x))
rd_subs = subjects[readmit]
length(rd_subs)


# check cdiff -------------------------------------------------------------

#Intestinal infection due to Clostridium difficile: ICD9_CODE = 845
head(cdat$ICD9_CODE)
c=table(ddat$ICD9_CODE)
sum(cdat$ICD9_CODE=='00845', na.rm=TRUE)
sum(ddat$ICD9_CODE=='00845')

cddat = cdat %>% 
  filter(ICD9_CODE=='00845')
length(unique(cddat$SUBJECT_ID))
length(unique(cddat$HADM_ID))
length(unique(cddat$HADM_ID)) / length(unique(cdat$HADM_ID)) *100
cdiff_subs = unique(cddat$SUBJECT_ID)


# overlap cdiff and return visits -----------------------------------------
length(rd_subs)
length(cdiff_subs)
sum(unique(cddat$SUBJECT_ID) %in% rd_subs)
subs = unique(cdat$SUBJECT_ID)
readmission = subs %in% rd_subs
cdiff = subs %in% cdiff_subs
fisher.test(x=readmission,
            y=cdiff)
cy_ry = sum(readmission & cdiff)
cy_rn = sum(!readmission & cdiff)
cn_ry = sum(readmission & !cdiff)
cn_rn = sum(!readmission & !cdiff)
c1 = c(cy_ry, cy_rn)
c2 = c(cn_ry, cn_rn)
tab = cbind(c1, c2)
tab
fisher.test(tab)
sum(tab)
length(subs)
cy_ry / sum(cdiff)
sum(readmission) / length(subs)
cn_ry / sum(!cdiff)

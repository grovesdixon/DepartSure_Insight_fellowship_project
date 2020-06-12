#build_outcomes_mimic.R
## Purpose is to make four outcome dataframes:
#Each outcome is based on HADM_IDs
#df1. cdiff_readmission: positive = readmitted within 30 days and diagnosed with C.diff during readmission
#df2. quick_cdiff_readmission: positive = readmitted within 30 days and has C.diff microbiology event within first 5 days
  #here remove all C.diff withiout microbiology events
#df3. readmission_after_cdiff: only among C. diff diagnosed cases: positive = readmitted within 30 days
#df4. any readmission: positive = readmitted within 30 days


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
         ORG_NAME,
         ORG_ITEMID,
         CHARTDATE,
         INTERPRETATION)
dat_list = list(adat, ddat, mdat)
cdat = purrr::reduce(dat_list, left_join)
dim(cdat)


# look at patient return --------------------------------------------------

#Sselect the cutoff for bad readmissions 
RETURN_CUT = 30

#total patients
length(unique(cdat$SUBJECT_ID))

#total return visits
vdat = cdat %>% 
  group_by(SUBJECT_ID) %>% 
  summarize(tvisists = length(unique(HADM_ID)))

#plot return visits
vdat %>% 
  mutate(return = tvisists > 1) %>% 
  ggplot(aes(x=tvisists, fill=return)) +
  geom_bar() +
  lims(x=c(0,6)) +
  labs(x='number of visits',
       y='number of patients') +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(breaks = c(1,2,3,4,5),
                     limits=c(0,6),
                     expand = c(0, 0)) +
  scale_fill_manual(values=c('grey50', 'firebrick3')) +
  theme(legend.position = 'none')
sum(vdat$tvisists > 1)
return_ids = vdat %>% 
  filter(tvisists > 1) %>% 
  pull(SUBJECT_ID)

#Subset for subjects with more than one hospital admission
rdat1 = cdat %>% 
  filter(SUBJECT_ID %in% return_ids) %>% 
  select(SUBJECT_ID,
         HADM_ID,
         ADMITTIME) %>% 
  filter(!duplicated(HADM_ID))
dim(rdat1)

#function to gather return visit data
get_returns_within_time = function(sid){
  sub = rdat1 %>% 
    filter(SUBJECT_ID==sid) %>% 
    arrange(ADMITTIME)
  sub$visit_number = 1:nrow(sub)
  v1s = data.frame(sub[1:(nrow(sub)-1),])
  v2s = data.frame(sub[2:nrow(sub),])
  vdat = v1s %>% 
    mutate(readmission_hadm_id = v2s$HADM_ID,
           re_admittime = v2s$ADMITTIME,
           readmission_visit_number = v2s$visit_number,
           interval = re_admittime - ADMITTIME)
  sub_keep = vdat %>% 
    select(SUBJECT_ID, HADM_ID, readmission_hadm_id, readmission_visit_number, interval)
  if (nrow(sub_keep) > 0){
    return(sub_keep)
  }
}

#pull out detailed re-admission data
return_list = map(unique(rdat1$SUBJECT_ID), function(x) get_returns_within_time(x))
all_return_df = purrr::reduce(return_list, rbind) %>% 
  as_tibble()
quick_return_df = all_return_df %>% 
  filter(as.numeric(interval) <= RETURN_CUT)
dim(quick_return_df)
save(quick_return_df, file='~/projects/Insight/icu_project/my_mimic/quick_return_df.Rdata')
ts = length(unique(cdat$SUBJECT_ID))
ta = length(unique(cdat$HADM_ID))
trs = length(unique(quick_return_df$SUBJECT_ID))
tra = length(unique(quick_return_df$HADM_ID))
trs #2030 total subjects readmitted
tra #2480 total readmissions
trs / ts * 100 #percentage of subjects readmitted
tra / ta * 100 #percentage of admissions that had a subsequent re-admission

#plot results
dim(all_return_df)
all_return_df %>% 
  mutate(early = as.numeric(interval) < 31) %>% 
  ggplot(aes(x=as.numeric(interval), fill=early)) +
  geom_histogram(bins=365) +
  labs(x='duration before readmission',
       y='readmitted patients') +
  scale_fill_manual(values=c('grey50', 'firebrick3')) +
  scale_x_continuous(breaks = c(0,30, 100,200,365), limits = c(0,366)) +
  theme(legend.position = 'none')


# compare diagnosis with microbiology events ------------------------------



# #COMPARE DIAGNOSES WITH MICROBIOLOGY EVENTS
# #do all C.diff diagnoses have a MICROBIOLOGY event?
# 
# #subset for cdiff diagnoses
cdiff_ddat = ddat %>%
  filter(ICD9_CODE=='00845')

# #subset for cdiff microbiology events
cdiff_mdat = mdat %>%
  filter(grepl('DIFF', ORG_NAME))

# 
# #RATE AT WHICH DIAGNOSES HAVE MICROBIOLOGY EVENTS
# sum(m_cdiff_admissions %in% cdiff_admissions) / length(cdiff_admissions) #65% of diagnoses have a microbiology event
# sum(!m_cdiff_admissions %in% cdiff_admissions) / length(cdiff_admissions)#some aren't
# 
# #RATE AT WHICH MICROBIOLOGY EVENTS AHVE
# sum(m_cdiff_admissions %in% cdiff_admissions) / length(m_cdiff_admissions) #most microbiology events are diagnosed
# sum(!m_cdiff_admissions %in% cdiff_admissions) / length(m_cdiff_admissions) #some aren't

# isolate df1 -------------------------------------------------------------
#df1. cdiff_readmission: positive = readmitted within 30 days and diagnosed with C.diff during readmission
#cdiff ICD9_CODE = 00845
quick_return_df
intervals = as.numeric(quick_return_df$interval)
names(intervals) = quick_return_df$HADM_ID
cdiff_ddat
ddat

#identify reasmissions that got cdiff
all_cdiff_admissions = unique(cdiff_ddat$HADM_ID)
readmissions_with_cdiff = quick_return_df %>% 
  filter(readmission_hadm_id %in% all_cdiff_admissions) %>% 
  pull(readmission_hadm_id)
  
df1 = adat %>% 
  left_join(all_return_df, by = c("SUBJECT_ID", "HADM_ID")) %>% 
  mutate(outcome = as.numeric(readmission_hadm_id %in% readmissions_with_cdiff))
t = table(df1$outcome)
t
t['1']/(t['0']+t['1'])*100


# isolate df2 -------------------------------------------------------------
#df2. quick_cdiff_readmission: positive = readmitted within 30 days and has C.diff microbiology event within first 5 days

#get admissions with micro in first 5 days
m5d_dat = cdiff_mdat %>% 
  left_join(adat, by = c('SUBJECT_ID', 'HADM_ID')) %>% 
  mutate(diff_minutes = (CHARTDATE-ADMITTIME),
         diff_days = diff_minutes/60/24,
         m5d = diff_days < 7)


#look at times
m5d_dat %>% 
  ggplot(aes(x=diff_days)) +
  geom_histogram()

#isolate within 5 days admissions
m5d_admissions = m5d_dat %>% 
  filter(m5d) %>% 
  pull(HADM_ID) %>% 
  unique()
length(m5d_admissions) #676 total cases with C.diff microbiology within first 7 days
sum(m5d_admissions %in% quick_return_df$readmission_hadm_id) #72 readmissions with C.diff within first 7 days


#BUILD THE OUTCOME DATAFRAME
#first isolate the cdiff cases to remove since we can't be sure they were aquired in previous admission
rm_cdiff_admissions0 = cdiff_ddat %>% 
  filter(!HADM_ID %in% m5d_admissions) %>% 
  pull(HADM_ID)
length(rm_cdiff_admissions0) #826
sum(rm_cdiff_admissions0 %in% m5d_admissions) #should be zero
rm_cdiff_admissions1 = cdiff_mdat %>% 
  filter(!HADM_ID %in% m5d_admissions) %>% 
  pull(HADM_ID)
length(rm_cdiff_admissions1)
sum(rm_cdiff_admissions0 %in% m5d_admissions)
rm_cdiff_admissions = unique(append(rm_cdiff_admissions0, rm_cdiff_admissions1))
length(rm_cdiff_admissions)

sum(quick_return_df$readmission_hadm_id %in% m5d_admissions)

#itentify positive cases when:
#readmission within 30 days and micro within 5 days
df2_pos_admissions = quick_return_df %>% 
  filter(readmission_hadm_id %in% m5d_admissions)
nrow(df2_pos_admissions)

#make final df
df2 = adat %>% 
  left_join(all_return_df, by = c("SUBJECT_ID", "HADM_ID")) %>% 
  filter(!readmission_hadm_id %in% rm_cdiff_admissions) %>% 
  mutate(return_interval = intervals[HADM_ID],
         outcome = as.numeric(readmission_hadm_id %in% df2_pos_admissions$readmission_hadm_id))
t = table(df2$outcome)
t
t['1']/(t['0']+t['1'])*100

# isolate df3 -----------------------------------------
#df3. readmission_after_cdiff: only among C. diff diagnosed cases: positive = readmitted within 30 days
#here I want the HADM_IDs where:
#1. diagnosis with C.diff
#2. call outcome of 0 for no readmit, 1 for yes

df3 = adat %>% 
  left_join(all_return_df, by = c("SUBJECT_ID", "HADM_ID")) %>% 
  filter(HADM_ID %in% cdiff_ddat$HADM_ID) %>% 
  mutate(outcome = as.numeric(readmission_hadm_id %in% quick_return_df$readmission_hadm_id))
t = table(df3$outcome)
t
t['1']/(t['0']+t['1'])*100


# isolate df4 -------------------------------------------------------------
#also exclude readmissions that took between 30 and 120 days
medium_readmissions = all_return_df %>% 
  filter(as.numeric(interval) < 120,
         as.numeric(interval) > 31)
min(medium_readmissions$interval)
max(medium_readmissions$interval)

df4 = adat %>% 
  left_join(all_return_df, by = c("SUBJECT_ID", "HADM_ID")) %>% 
  filter(!HADM_ID %in% medium_readmissions$HADM_ID,
         HOSPITAL_EXPIRE_FLAG==0) %>% 
  mutate(outcome = as.numeric(readmission_hadm_id %in% quick_return_df$readmission_hadm_id))
sum(df4$outcome)
t = table(df4$outcome)
t
t['1']/(t['0']+t['1'])*100



# write out the results ---------------------------------------------------
#df1. cdiff_readmission: positive = readmitted within 30 days and diagnosed with C.diff during readmission
#df2. quick_cdiff_readmission: positive = readmitted within 30 days and has C.diff microbiology event within first 5 days
#here remove all C.diff withiout microbiology events
#df3. readmission_after_cdiff: only among C. diff diagnosed cases: positive = readmitted within 30 days
#df4. any readmission: positive = readmitted within 30 days

#note for all of these, HADM_ID is the FIRST visit. This is the one we have data for when forecasting

df1 %>% 
  write_csv('~/projects/Insight/icu_project/my_mimic/readmission_cdiff.csv')
df2 %>% 
  write_csv('~/projects/Insight/icu_project/my_mimic/readmission_quickCdiff.csv')
df3 %>% 
  write_csv('~/projects/Insight/icu_project/my_mimic/readmission_after_cdiff.csv')
df4 %>% 
  write_csv('~/projects/Insight/icu_project/my_mimic/any_readmission.csv')
dim(df1)
dim(df2)
dim(df3)
dim(df4)

# isolate reasmission with c.diff ------------------------------------------------
#here we want HADM_IDs where:
#1 patient was readmitted within 30 days and diagnosed with C.diff in first 48 hours

#FIRST CHECK FOR THE DIAGNOSIS AT RE-ADMISSION
#this uses the diagnosis column in the admissions table
d_strings = unique(adat$DIAGNOSIS)
length(d_strings)
sum(grepl('IFF', d_strings))
diff_strings = d_strings[grep('DIFF', d_strings)]
cdiff_strings = diff_strings[!diff_strings %in% c("DIFFUSED INFILTRATES",
                                            "RESPIRATORY DIFFICULTY",
                                            "MITRAL VALVE DIFFIENCY\\MITRAL VALVE REPLACEMENT /SDA")]
length(cdiff_strings)#so this won't work





# check cdiff -------------------------------------------------------------

#Intestinal infection due to Clostridium difficile: ICD9_CODE = 00845
head(cdat$ICD9_CODE)
c=table(ddat$ICD9_CODE)
sum(cdat$ICD9_CODE=='00845', na.rm=TRUE)

#subset for patients with C.diff
cddat = cdat %>% 
  filter(ICD9_CODE=='00845')
length(unique(cddat$SUBJECT_ID))
length(unique(cddat$HADM_ID))
length(unique(cddat$HADM_ID)) / length(unique(cdat$HADM_ID)) *100
cdiff_admissions = unique(cddat$HADM_ID)
length(cdiff_admissions)

# overlap cdiff and return visits -----------------------------------------
length(rd_subs)
length(cdiff_subs)
subs = unique(cdat$SUBJECT_ID)
total_cdiff_reads = sum(unique(cddat$SUBJECT_ID) %in% rd_subs)
total_cdiff_reads
total_cdiff_reads / length(subs)

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

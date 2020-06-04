#patient.R
source('explore/explore.R')

#load data
edat = load_csv(eicu_path, 'patient.csv.gz')
mdat0 = read_csv(paste(mimic_path, 'PATIENTS.csv', sep=''))
mdat1 = read_csv(paste(mimic_path, 'ADMISSIONS.csv', sep=''))

#total distinct patients
colnames(edat)
colnames(mdat0) #mimic pateints
colnames(mdat1) #mimic admissions
length(unique(edat$patienthealthsystemstayid)) / 1e3
length(unique(mdat$SUBJECT_ID)) / 1e3

#combine MIMIC data and format features:
#age, ethnicity
mdat = mdat0 %>% 
  left_join(mdat1, by = 'SUBJECT_ID') %>% 
  mutate(age =difftime(ADMITTIME, DOB),
         age = as.duration(age),
         age = as.numeric(age, 'year'))

mimi_ethnicities = unique(mdat$ETHNICITY) %>% 
  data.frame() %>% 
  write_csv('explore/ethnicities.csv')

#total icu visits
length(unique(edat$patientunitstayid)) / 1e3
length(unique(mdat$HADM_ID))


#visits accross time eic
max_to_view = 8
v_counts = table(edat$patienthealthsystemstayid)
evplt = v_counts %>% 
  data.frame() %>% 
  as_tibble() %>% 
  ggplot(aes(x=Freq)) +
  geom_bar() +
  scale_x_continuous(breaks = 0:max_to_view, label = 0:max_to_view, limits=c(0,max_to_view)) +
  labs(x='N visits',
       subtitle='EICU')

#total return visits
sum(v_counts>1) / 1e3

#visits accross time mimic
mv_counts = table(mdat$SUBJECT_ID)
mvplt = mv_counts %>% 
  data.frame() %>% 
  as_tibble() %>% 
  ggplot(aes(x=Freq)) +
  geom_bar() +
  scale_x_continuous(breaks = 0:max_to_view, label = 0:max_to_view, limits=c(0,max_to_view)) +
  labs(x='N visits',
       subtitle='MIMIC')
plot_grid(evplt, mvplt, nrow=1)

#mimic return visits
sum(mv_counts>1) / 1e3

#total
(sum(mv_counts>1) + sum(v_counts>1)) / 1e3

#look at age
eage = edat %>% 
  filter(!is.na(age)) %>% 
  mutate(age = if_else(age=="> 89",
                       '91',
                       age),
         age = as.numeric(age)) %>% 
  filter(age > 18) %>% 
  select(patienthealthsystemstayid, age, gender, ethnicity) %>% 
  mutate(dataset='EICU') %>% 
  set_names(c('id', 'age', 'gender', 'ethnicity', 'dataset'))

mage = mdat %>% 
  mutate(age = if_else(age > 200,
                       91,
                       age)) %>% 
  mutate(GENDER = if_else(GENDER=='F',
                          'Female',
                          GENDER),
         GENDER = if_else(GENDER=='M',
                          'Male',
                          GENDER)) %>% 
  filter(age > 18) %>% 
  select(SUBJECT_ID, age, GENDER, ETHNICITY) %>% 
  mutate(dataset='MIMIC') %>% 
  set_names(c('id', 'age', 'gender', 'ethnicity', 'dataset'))

#combine and merge ethnicity
cdat = rbind(eage, mage)
unique(edat$ethnicity)
unique(mdat$ETHNICITY)
start = unique(cdat$ethnicity)
target = start
target[grep('^WHITE', start)]<-"Caucasian"
target[start=="PORTUGUESE"]<-"Caucasian"
target[grep('^BLACK', start)]<-"African American"
target[grep('^HISPANIC', start)]<-"Hispanic"
target[start=="SOUTH AMERICAN"]<-"Hispanic"
target[grep('^ASIAN', start)]<-"Asian"
target[grep('^AMERICAN INDIAN', start)]<-"Native American"
other_unknown = c("UNKNOWN/NOT SPECIFIED",
                  "OTHER",
                  "PATIENT DECLINED TO ANSWER",
                  "UNABLE TO OBTAIN",
                  "MULTI RACE ETHNICITY",
                  "CARIBBEAN ISLAND",
                  "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
                  "MIDDLE EASTERN")
target[start %in% other_unknown]<-"Other/Unknown"
ut = unique(target)
ut[!ut %in% unique(edat$ethnicity)]
translate = target
names(translate) = start
cdat$ethnicity = translate[cdat$ethnicity]
cdat$ethnicity[is.na(cdat$ethnicity)]<-"Other/Unknown"
unique(cdat$ethnicity)

#plot age
cdat %>% 
  ggplot(aes(x=age)) +
  geom_histogram(bins=73) +
  facet_wrap(~dataset, scales = 'free')

#plot gender
cdat %>% 
  mutate(gender = if_else(!gender %in% c('Female', 'Male'),
                          'Unknown',
                          gender)) %>% 
  ggplot(aes(x=gender)) +
  geom_bar() +
  facet_wrap(~dataset, scales = 'free')

#plot ethnicity
cdat %>% 
  ggplot(aes(x=ethnicity)) +
  geom_bar() +
  facet_wrap(~dataset, scales = 'free')


#weird values in age?
unique(edat$age)

edat %>% 
  filter(is.na(age))




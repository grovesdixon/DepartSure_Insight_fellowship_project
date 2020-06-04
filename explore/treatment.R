#treatment.R
source('explore_eicu/explore.R')

#load treatment data
tdat = load_csv(data_path, 'treatment.csv.gz')
tdat


#subset for pateint as in demo notebook
sid = 242040
df = tdat %>% 
  filter(patientunitstayid==sid)
df
dim(df)


#look at treatments
treatments = df$treatmentstring

dft = df %>% 
  mutate(treat_factor = factor(treatmentstring),
         treat_num = factor(as.numeric(treat_factor)))
dft %>% 
  ggplot(aes(x=treatmentoffset, y=treat_num, color=treat_num)) +
  geom_point() +
  scale_y_discrete(labels = unique(dft$treatmentstring))

table(df$treatmentstring)


# how representative is the treatment table? ------------------------------
pdat = load_csv(data_path, 'patient.csv.gz')
colnames(tdat)
colnames(pdat)

#how much overlap?
pids = pdat$patientunitstayid
tids = tdat$patientunitstayid
sum(pids %in% tids)

#what is the level of completeness of the treatment data?
#(ie what proportion of all patients from the hospitals have a treatment entry?)
hcounts = pdat %>% 
  mutate(hasTreat = patientunitstayid %in% tdat$patientunitstayid) %>% 
  select(hospitalid, patientunitstayid, hasTreat) %>% 
  filter(!duplicated(patientunitstayid)) %>% 
  group_by(hospitalid) %>% 
  summarize(nPatients = n(),
            nWtreat = sum(hasTreat)) %>% 
  mutate(completion = nWtreat / nPatients*100)
hcounts %>% 
  ggplot(aes(x=completion)) +
  geom_histogram(bins=10) +
  labs(subtitle='data completion rates')

  




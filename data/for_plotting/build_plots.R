#build_plots.R
#build plots for presentation

library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())


# plot AUC comparison -----------------------------------------------------

auc_dat = read_csv('data/for_plotting/auc_comparison.csv')
auc_dat %>% 
  mutate(model = factor(model, levels = c('random forest', 'logistic regression'))) %>% 
  ggplot(aes(x=model, y=auc)) +
  geom_bar(stat='identity') +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0,0.8, 0.4),
                     limits=c(0,0.85)) +
  labs(y='AUC',
       x='') +
  coord_flip()



# plot correlation histograms ---------------------------------------------
THRESHOLD = 0.2
data_dir= '~/gitreps/Insight_fellowship_project/data/for_plotting/'

plot_coefs = function(data_dir, data_file){
  data_path = paste(data_dir, data_file, sep='')
  cdf = read_csv(data_path)
  tot_selected = sum(abs(cdf$coef)>THRESHOLD)
  print(paste('total coefficients selected =', tot_selected))
  plt = cdf %>% 
    mutate(selected=abs(coef)>0.5) %>% 
    ggplot(aes(x=coef, fill=selected)) +
    geom_histogram(bins=80) +
    scale_fill_manual(values = c('grey50', 'firebrick3')) +
    labs(x='coefficients',
         y='feature count') +
    theme(legend.position = 'none')
  pbuild = ggplot_build(plt)
  yrange = pbuild$layout$panel_params[[1]]$y.range
  xrange = pbuild$layout$panel_params[[1]]$x.range
  plt +
    annotate("text", x = xrange[1], y = yrange[2],
             label = paste('N ==', tot_selected), parse=TRUE, color='firebrick3',
             hjust=0, size=4)
}

#diagnoses
plot_coefs(data_dir, 'diagnosis_icd9_coefs.csv')
#procedures
plot_coefs(data_dir, 'procedure_icd9_coefs.csv')
#dugs
plot_coefs(data_dir, 'drug_coefs.csv')
#discharge notes
plot_coefs(data_dir, 'dischargeNotes_coefs.csv') + scale_x_continuous(breaks=c(-0.2, 0, 0.2, 0.4))


####Assess metric response to stress

library(tidyverse)
library(ggplot2)

#Load GIS data

#Load habitat data
phab_df<-read_csv("Data/NonBioData/Habitat/PHAB_Metrics_07252023.csv") %>%
  mutate(ReachStress = case_when(HumanActivity_Ext<12 &
                                   HumanActivity_Int<12 & 
                                   # HumanActivity_Prox<8 &
                                   HumanActivity_Prox_SWAMP<6 ~0,
                                 HumanActivity_Ext>=20~2,
                                 HumanActivity_Int>=20~2,
                                 # HumanActivity_Prox>=20~2,
                                 HumanActivity_Prox_SWAMP>=10~2,
                                 T~1 ))

ref_sites<-phab_df %>%
  group_by(StationCode) %>%
  summarise(ReachStressMax = max(ReachStress, na.rm=T)) %>%
  ungroup() %>%
  mutate(StressLevel = case_when(ReachStressMax==0~"Low",
                                 ReachStressMax==1~"Med",
                                 ReachStressMax==2~"High",
                                 T~"Other"))
ref_sites %>% group_by(StressLevel) %>% tally()
#Load bio data, for assessing sensitivity
bug_mets_lu<-read_csv("Data/BioData/Arthros/bug_metric_lu_07252023.csv") %>%
  mutate(Metric_short=str_sub(Metric, 3, nchar(Metric)))

arthro_df<-read_csv("Data/BioData/Arthros/arthropod_metircs_07252023.csv")
arth_df_long<-arthro_df %>%
  select(StationCode, SampleDate, CollectionMethodCode, all_of(bug_mets_lu$Metric_short)) %>%
  pivot_longer(cols=all_of(bug_mets_lu$Metric_short), 
               names_to="Metric_short", 
               values_to="MetricValue", 
               values_drop_na = T) %>%
  mutate(Metric = case_when(CollectionMethodCode=="TerInvt_T_DS"~paste0("T_",Metric_short),
                            CollectionMethodCode=="TerInvt_V_DS"~paste0("V_",Metric_short),
                            T~Metric_short)) %>%
  #Get rid of these interim calc metrics
  filter(!str_detect(Metric,"KRich")) %>%
  filter(!str_detect(Metric,"KAbund")) %>%
  inner_join(bug_mets_lu %>% select(Metric, Method, MetricForm, MetricGroup=Group))

bryo_mets_lu<-read_csv("Data/BioData/Bryos/bryo_metric_lu.csv")
bryo_df<-read_csv("Data/BioData/Bryos/bryo_metrics_07262023_v2.csv")
# bryo_df<-read_csv("Data/BioData/Bryos/Final_Bryophyte_Output.csv") 
skimr::skim(bryo_df)

bryo_df_long<-bryo_df %>%
  select(-SampleID, -CollectionMethodCode, -Replicate) %>%
  pivot_longer(cols=c(-StationCode, -SampleDate),
               names_to = "Metric_short", values_to="MetricValue", values_drop_na = T) %>%
  filter(MetricValue > -Inf & MetricValue<Inf) %>%
  mutate(CollectionMethodCode="Bryophyte",
         Method="Bryo",
         MetricForm="Bryo", MetricGroup="Bryo", #Until I get lookup
         Metric= paste0("B_",Metric_short))



junk<-lm(MetricValue~StressLevel, 
         data=arth_df_long %>% na.omit() %>%
           filter(MetricValue>-Inf) %>%
           left_join(ref_sites %>% select(StationCode, StressLevel)))
junk<-t.test(
  MetricValue~StressLevel, 
  data=
    arth_df_long %>% 
    filter(MetricValue>-Inf) %>%
    inner_join(
      ref_sites %>% 
        select(StationCode, StressLevel) ) %>%
    filter(StressLevel %in% c("Low","High"))
)
junk %>% broom::glance()

junk2<-junk %>% broom::glance()
junk2$f

metric_summary<-arth_df_long %>%
  bind_rows(bryo_df_long) %>%
  filter(MetricValue>-Inf) %>%
  filter(!str_detect(Metric_short, "_KRich")) %>%
  filter(!str_detect(Metric_short, "_KAbund")) %>%
  left_join(ref_sites %>% select(StationCode, StressLevel)) %>%
  group_by(Metric, Metric_short, MetricGroup, MetricForm) %>%
  summarise(
    unique_values = unique(MetricValue) %>% length(),
    F_stat = lm(MetricValue~StressLevel) %>%
      broom::glance() %>%
      select(statistic) %>% 
      unlist(),
    
    # t_stat = t.test(MetricValue~StressLevel, 
    #                 data= .%>% filter(StressLevel!="Med")) %>%
    #   broom::glance() %>%
    #   select(statistic) %>% 
    #   unlist()
  ) %>%
  ungroup() 

metric_summary$t_stat <-sapply(1:nrow(metric_summary), function(i){
  vals.i=metric_summary$unique_values[i]
  if(vals.i==1)
    0
  else
  {
    met.i = metric_summary$Metric[i]
    xdf = arth_df_long %>%
      bind_rows(bryo_df_long) %>%
      filter(MetricValue>-Inf) %>%
      filter(Metric==met.i) %>%
      left_join(ref_sites %>% select(StationCode, StressLevel)) %>%
      filter(StressLevel %in% c("Low","High"))
    t_xdf = t.test(MetricValue~StressLevel, data = xdf)
    t_xdf$statistic
  }
})

ggplot(metric_summary %>%
         filter(unique_values>1), aes(x=F_stat))+
  geom_histogram()+
  facet_wrap(~MetricForm)+
  geom_vline(xintercept=2, linetype="dashed", color="red")


ggplot(metric_summary %>%
         filter(unique_values>1) %>%
         pivot_longer(cols=c(F_stat, t_stat))
       , aes(x=abs(value)))+
  geom_density(aes(fill=name), alpha=.5)+
  facet_wrap(~MetricForm)+
  geom_vline(xintercept=2, linetype="dashed", color="red")


metric_summary %>%
  filter(F_stat > 10)




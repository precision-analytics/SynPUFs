#ascertain outcome and define cohort exit date (index date for case)

#since outcome is defined as hospitalization of acute kidney injury, cases can be ascertained from 
#inpatient data

acute.kidney<-c('584','5845','5846','5847','5848','5849')

case<-inpatient_diag%>%filter(DESYNPUF_ID %in% study_population$DESYNPUF_ID)%>%
                       filter(diag %in% acute.kidney|ADMTNG_ICD9_DGNS_CD %in% acute.kidney)

#define index date as the hospital discharge date for first-time diagnosis:
case<-case%>%group_by(DESYNPUF_ID)%>%summarise(index_date=min(NCH_BENE_DSCHRG_DT))

#remove individuals with acute kidney injury before cohort entry:
case<-case%>%left_join(study_population[,c('DESYNPUF_ID','cohort_entry')])%>%
            filter(cohort_entry<index_date)

#exclude individuals with previous acute kidney injury?

#merge index_date to study_population:
study_population%<>%left_join(case[,1:2])
study_population$outcome<-ifelse(is.na(study_population$index_date),0,1)



#define study exit date for controls:
#end of follow-up defined as date of death, 24 months after statin treatment or 2010-12-31
study_population<-study_population%>%
                  mutate(exit_date=case_when(!is.na(index_date)~index_date,
                                             !is.na(BENE_DEATH_DT)~BENE_DEATH_DT,
                                             TRUE~ ymd('2010-12-31')))
study_population$M24<-study_population$cohort_entry+730
study_population$exit_date<-if_else(study_population$M24<=study_population$cohort_entry,
                                  study_population$M24,study_population$exit_date)
#clean up columns:
study_population%<>%select(-M24,-index_date)

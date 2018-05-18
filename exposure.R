#define exposure:
#define low/high potency statin treatment:
#subset prescription information between entry and exit date:
statin_user<-prescription%>%filter(DESYNPUF_ID %in% study_population$DESYNPUF_ID,
                                   PROD_SRVC_ID %in% statin$ndc_code)
statin_user<-left_join(statin_user,study_population[,c('DESYNPUF_ID','cohort_entry','exit_date')])%>%
             filter(SRVC_DT>=cohort_entry,SRVC_DT<=exit_date)

#map dosage to prescription:
statin_cat<-statin%>%
            filter(ndc_code %in% statin_user$PROD_SRVC_ID)%>%
            select(ndc_code,NONPROPRIETARYNAME,ACTIVE_NUMERATOR_STRENGTH)

#deal with drugs with multiple ingredients:
multi_med<-statin_cat%>%filter(grepl(';',ACTIVE_NUMERATOR_STRENGTH))%>%
                        mutate(NONPROPRIETARYNAME=gsub('and',';',NONPROPRIETARYNAME))%>%
                        separate_rows(NONPROPRIETARYNAME,ACTIVE_NUMERATOR_STRENGTH,sep=';')%>%
                        filter(grepl('statin',NONPROPRIETARYNAME))

statin_cat<-statin_cat%>%filter(ndc_code %ni% multi_med$ndc_code)%>%
                         rbind(multi_med)
rm(multi_med)

#create high/low potency level of statins:
#Definition:
#high potency:
#rosuvastatin>=20mg;simvastatin>=40 mg
statin_cat$ACTIVE_NUMERATOR_STRENGTH<-as.integer(statin_cat$ACTIVE_NUMERATOR_STRENGTH)
statin_cat<-statin_cat%>%
            mutate(level=case_when(grepl('rosuvastatin',NONPROPRIETARYNAME)&ACTIVE_NUMERATOR_STRENGTH>=20~ 'high',
                                   grepl('simvastatin',NONPROPRIETARYNAME)&ACTIVE_NUMERATOR_STRENGTH>=40 ~ 'high',
                                   TRUE ~ 'low'))


statin_user<-statin_user%>%left_join(statin_cat[,c(1,4)],by=c('PROD_SRVC_ID'='ndc_code')) 

#calculate cumulative duration of exposure:
statin_user<-statin_user%>%group_by(DESYNPUF_ID)%>%
                           mutate(duration=sum(DAYS_SUPLY_NUM))%>%
                           ungroup()
#categorize duration into three categories: <-120, 121-365, 366-730:
statin_user$duration<-cut(statin_user$duration,breaks=c(0,120,365,730,Inf),labels=c('Below 120','121-365','366-730','Above 730'))


#define current user or past user: 
#current user defined as The end date of last dispense of statin was within 120 days of the index date(study end date)
statin_recency<-statin_user%>%
                group_by(DESYNPUF_ID)%>%
                filter(SRVC_DT==max(SRVC_DT))%>%
                mutate(recency=ifelse(SRVC_DT+120+DAYS_SUPLY_NUM<=exit_date,'past','current'))%>%
                ungroup()

#for individuals who received both high and low dose of statin in past or current category, the exposure was categorized as high:
#if two level of statin potency exists in a patient, recode the level to high
statin_level<-statin_user%>%group_by(DESYNPUF_ID)%>%
              summarise(n_level=n_distinct(level))%>%
              filter(n_level>1)

statin_user$level[statin_user$DESYNPUF_ID %in% statin_level$DESYNPUF_ID]<-'high'

statin_user<-statin_user%>%distinct(DESYNPUF_ID,level,duration)%>%
             left_join(statin_recency[,c('DESYNPUF_ID','recency')])

#bind exposure table to study_population:
study_population%<>%left_join(statin_user)
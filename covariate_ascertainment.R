#identify covariates for propensity score calculation/matching:
#search in inpatient_diag and outpatient_diag 6 months before individuals' entry into cohort

#function for defining covariate:
define_cov<-function(df,covariate,date){
  df%>%filter(grepl(paste(covariate,collapse='|'),diag)|
                grepl(paste(covariate,collapse='|'),ADMTNG_ICD9_DGNS_CD))%>%
    left_join(study_population)%>%
    filter(!!date<cohort_entry & !!date>cohort_entry-180)%>%
    distinct(DESYNPUF_ID)%>%
    pull()
}

#use data source: inpatient and outpatient diagnostic code
#code for chronic kidney disease
ckd<-c('^585\\d{1}','\\b586\\b','^403\\d{1}1','^404\\d{1}2','^404\\d{1}3','^582\\d{,2}',
       '^583\\d{,1}','\\b587\\b','^588\\d{,1}')

  
ckd<-unique(c(define_cov(inpatient_diag,ckd,quo(NCH_BENE_DSCHRG_DT)),
              define_cov(outpatient_diag,ckd,quo(CLM_THRU_DT))))


#hypertensive disease
hpt<-c('^401\\d{1}','^402\\d{,2}','^403\\d{,2}','^404\\d{,2}','^405\\d{,2}')
hpt<-unique(c(define_cov(inpatient_diag,hpt,quo(NCH_BENE_DSCHRG_DT)),
              define_cov(outpatient_diag,hpt,quo(CLM_THRU_DT))))

#hyperchloesterolemia
hpc<-c('^272\\d{1}')
hpc<-unique(c(define_cov(inpatient_diag,hpc,quo(NCH_BENE_DSCHRG_DT)),
              define_cov(outpatient_diag,hpc,quo(CLM_THRU_DT))))

#peripheral vascular disease
pvd<-c('^433\\d{,2}')
pvd<-unique(c(define_cov(inpatient_diag,pvd,quo(NCH_BENE_DSCHRG_DT)),
              define_cov(outpatient_diag,pvd,quo(CLM_THRU_DT))))

#heart failure
hf<-c('^428\\d{,2}')
hf<-unique(c(define_cov(inpatient_diag,hf,quo(NCH_BENE_DSCHRG_DT)),
             define_cov(outpatient_diag,hf,quo(CLM_THRU_DT))))

#injury and poisoning
injury<-paste0('^',as.character(c(800:999)),'\\d{,2}')
injury<-unique(c(define_cov(inpatient_diag,injury,quo(NCH_BENE_DSCHRG_DT)),
             define_cov(outpatient_diag,injury,quo(CLM_THRU_DT))))

#number of physician visits:

#greater than 4 distinct drug (not including dosage change):
#remove last two digit of drug code for package difference
drugcount<-prescription%>%filter(DESYNPUF_ID %in% study_population$DESYNPUF_ID)%>%
           left_join(study_population[c('DESYNPUF_ID','cohort_entry')])%>%
           filter(cohort_entry>SRVC_DT & SRVC_DT>cohort_entry-180)%>%
           mutate(drug_code=substr(PROD_SRVC_ID,1,nchar(PROD_SRVC_ID)-2))%>%
           group_by(DESYNPUF_ID)%>%
           summarise(n_drug=n_distinct(drug_code))%>%
           filter(n_drug>4)%>%
           distinct(DESYNPUF_ID)%>%
           pull()


#greater than 4 physician visit (outpatient):
#hcpcs code 99213 used to define middle office or other outpatient established office patient visit
visit<-outpatient_hcpcs%>%filter(DESYNPUF_ID%in% study_population$DESYNPUF_ID,
                                 hcpcs=='99213')%>%
                          left_join(study_population[,c('DESYNPUF_ID','cohort_entry')])%>%
                          filter(CLM_THRU_DT<cohort_entry & CLM_THRU_DT>cohort_entry-180)%>%
                          group_by(DESYNPUF_ID)%>%
                          summarise(n_visit=n_distinct(CLM_ID))%>%
                          filter(n_visit>=4)%>%
                          distinct(DESYNPUF_ID)%>%
                          pull()

#hospitalization
hosp<-inpatient%>%filter(DESYNPUF_ID%in% study_population$DESYNPUF_ID)%>%
                  left_join(study_population[,c('DESYNPUF_ID','cohort_entry')])%>%
                  filter(NCH_BENE_DSCHRG_DT<cohort_entry & NCH_BENE_DSCHRG_DT>cohort_entry-180)%>%
                  distinct(DESYNPUF_ID)%>%
                  pull()

#laboratory test:
#laboratory testing can be ascertained from outpatient service hcpcs code
#hcpcs Code range (80047-89398)
lab<-as.character(c(80047:89398))
lab<-outpatient_hcpcs%>%filter(DESYNPUF_ID%in% study_population$DESYNPUF_ID,
                               hcpcs %in% lab)%>%
                        left_join(study_population[,c('DESYNPUF_ID','cohort_entry')])%>%
                        filter(CLM_THRU_DT>cohort_entry-180 & CLM_THRU_DT < cohort_entry)%>%
                        distinct(DESYNPUF_ID)%>%
                        pull()

#create new columns in the study population dataset for all covariates:
assign_cova<-function(df,covariate,covariate_name){
  df2<-mutate(df, !!covariate_name:=ifelse(study_population$DESYNPUF_ID %in% covariate,1,0))
  return(df2)
}


covariates<-list(ckd,hpt,hpc,hf,pvd,injury,drugcount,visit,hosp,lab)
covariate_name<-c('ckd','hypertension','hypercholesterol','heart.failure','PVD','injury.poison','drug.dispense','doctor.visit',
                  'hospital','lab.test')

for (i in seq_along(covariate_name)){
  study_population<-assign_cova(study_population,covariates[[i]],covariate_name[i])
}

rm(covariates,covariate_name,ckd,hpt,hpc,hf,pvd,injury,drugcount,visit,hosp,lab)

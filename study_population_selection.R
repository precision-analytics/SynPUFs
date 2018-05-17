#inclusion/exclusion criteria:

#clean-up ndc table from FDA:
# ndc<-read.csv('../dataset/ndcxls/product.csv',stringsAsFactors = F)
# ndc_product<-read.csv('../dataset/ndcxls/package.csv',stringsAsFactors = F)
# colnames(ndc_product)<-c('PRODUCTNDC','ndc_code')
# ndc<-ndc[,c(2,4,6,14,15,16,17)]
# ndc%<>%left_join(ndc_product)
# ndc$ndc_code<-gsub('-|^0*','',ndc$ndc_code,perl = T)
# ndc$NONPROPRIETARYNAME<-tolower(ndc$NONPROPRIETARYNAME)
# saveRDS(ndc,'../dataset/ndc.RData') #save the cleaned data for later use
ndc<-readRDS('../dataset/ndc.RData')
#######################################################################################################

#Apply exclusion criteria:
#1. select individuals aged above 40 during their first year in the database:
beneficiary<-beneficiary%>%group_by(DESYNPUF_ID)%>%
                           mutate(year_entry=min(year),
                                  age_entry=min(year)-year(BENE_BIRTH_DT))%>%
                           ungroup()
beneficiary%<>%filter(age_entry>=40)

#2. exclude individuals with less than 1 year medical information: (died within one year)
beneficiary<-beneficiary%>%mutate(fu=as.numeric(BENE_DEATH_DT-ymd('2008-01-01')))
                          
id_exclude<-beneficiary%>%filter(fu<365)%>%distinct(DESYNPUF_ID)
`%ni%`<-Negate('%in%')
beneficiary<-beneficiary%>%filter(DESYNPUF_ID %ni% id_exclude$DESYNPUF_ID)%>%
                           select(-fu)

#retrieve substance name from C10 ATC category to define lipid-lowering drugs
source('~/Google Drive/R/Interesting excercises/whoatc.R')
atc<-c('C10AA','C10AB','C10AC','C10AD','C10AX','C10BA','C10BX')
lpl<-list()
for (i in seq_along(atc)){
  lpl[[i]]<-whoatc(atc[i])
}

lipid_low<-do.call(rbind,lpl)
lipid_low%<>%filter(nchar(code)==7)

#select drugs with nonproprietaryname in lipid lowering agents:
lipid_low<-ndc%>%filter(grepl(paste(lipid_low$desc,collapse = '|'),NONPROPRIETARYNAME))
niacin<-ndc%>%filter(NONPROPRIETARYNAME=='niacin')

# include statin users and define cohort entry date:
statin<-lipid_low%>%filter(grepl('statin',NONPROPRIETARYNAME))

statin_user<-prescription%>%filter(PROD_SRVC_ID %in% statin$ndc_code)%>%
  group_by(DESYNPUF_ID)%>%
  summarise(cohort_entry=min(SRVC_DT))

#define study population from inclusion criteria:
study_population<-beneficiary%>%
  left_join(statin_user)%>%
  filter(!is.na(cohort_entry))%>%
  distinct()

#3. exclude individuals whose first lipid-lowering or niacin prescription was within 365 days from 2008-01-01:

#from prescription data, select individuals who had a prescription of lipid-lowering drug or niacin 
#select individuals whose first prescription is within one year from 2018-01-01:
exclusion2<-prescription%>%filter(PROD_SRVC_ID %in% c(lipid_low$ndc_code,niacin$ndc_code))%>%
                           group_by(DESYNPUF_ID)%>%
                           summarise(first_med=min(SRVC_DT))%>%
                           filter(first_med<ymd('2009-01-01'))

study_population%<>%filter(DESYNPUF_ID %ni% exclusion2$DESYNPUF_ID)
rm(exclusion,exclusion2,lpl,beneficiary)

#4. exclude individuals with dialysis in previous year before cohort entry:

# dialysis can be ascertained from inpatient, outpatient and carrier procedure code:CPT codes 90935 and 90937
# 90945, and 90947  HCPCS code Z6042) or icd-9 code: 39.95
dialysis_icd<-c('3995')
dialysis_ex<-inpatient_proc%>%filter(proc %in% dialysis_icd,DESYNPUF_ID%in% study_population$DESYNPUF_ID)%>%
             left_join(study_population[,c('DESYNPUF_ID','cohort_entry')])%>%
             filter(NCH_BENE_DSCHRG_DT<cohort_entry)

#check in outpatient data
dialysis<-c('90935','90937','90945','90947')
dialysis_ex2<-outpatient_hcpcs%>%filter(hcpcs %in% dialysis,DESYNPUF_ID%in% study_population$DESYNPUF_ID)%>%
              left_join(study_population[,c('DESYNPUF_ID','cohort_entry')])%>%
              filter(CLM_THRU_DT<cohort_entry)

dialysis_ex3<-outpatient_proc%>%filter(proc %in% dialysis_icd,DESYNPUF_ID%in% study_population$DESYNPUF_ID)%>%
  left_join(study_population[,c('DESYNPUF_ID','cohort_entry')])%>%
  filter(CLM_THRU_DT<cohort_entry)


study_population<-study_population%>%
                  filter(DESYNPUF_ID %ni% c(dialysis_ex$DESYNPUF_ID,dialysis_ex2$DESYNPUF_ID,
                                                               dialysis_ex3$DESYNPUF_ID))

study_population%<>%select(-year,-year_entry)%<>%distinct()
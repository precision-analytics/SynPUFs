#this files aimed for initial data cleaning
library(bit64)
library(magrittr)
library(lubridate)
library(dplyr)
library(tidyr)
library(xlsx)
beneficiary<-readRDS('../dataset/beneficiary.RData')

#print out first 5 observations from beneficiary dataset
head(beneficiary)

#convert date variable into date format:
beneficiary[,c(2,3)]<-lapply(beneficiary[,c(2,3)],ymd)

#variable 24-32 are cost-related variable, remove since they won't be used in this study
#remove column 13:23, as those chronic disease ascertainment not relevant for the study
#remove column 6:8 for geographic information
#remove column 9:12 for insurance coverage information, not relevant
beneficiary<-beneficiary[,c(1:5,33)]

#summarise 3 year beneficiary data into one row per individual:
#summarise death date, if appliable:
death<-beneficiary%>%filter(!is.na(BENE_DEATH_DT))
beneficiary<-beneficiary%>%distinct(DESYNPUF_ID,BENE_BIRTH_DT,BENE_SEX_IDENT_CD,BENE_RACE_CD,year)%>%
                           left_join(death[,c('DESYNPUF_ID','BENE_DEATH_DT')])
rm(death)
############################################################################################################################
inpatient<-readRDS('../dataset/inpatient.RData')
head(inpatient)
inpatient[,c(4,5,12,19)]<-lapply(inpatient[,c(4,5,12,19)],ymd)
#remove variable that are not useful for the study
inpatient<-inpatient[,c(1,2,4:5,12,13,19,21:81)]

#convert data from wide format to long format, separated by diagnostic code, procedure code and hcpcs code:
#The final table will be in the form of one diagnostic/procedure/hcpcs code per row
#keep id, admission/discharge date in all data
inpatient_diag<-inpatient%>%select(1:17)%>%
                            gather(diag_cat,diag,starts_with('ICD9_DGNS'),na.rm=T)%>%
                            filter(diag!='') #remove blank cell and NAs 

inpatient_proc<-inpatient%>%select(c(1:7,starts_with('ICD9_PRCDR')))%>%
                            gather(proc_cat,proc,starts_with('ICD9_PRCDR'),na.rm=T)%>%
                            filter(proc!='')

inpatient_hcpcs<-inpatient%>%select(c(1:7,starts_with('HCPCS')))%>%
                            gather(hcpcs_cat,hcpcs,starts_with('HCPCS'),na.rm=T) 
#it seems that no hcpcs code is available for inpatient data!!!
rm(inpatient)
rm(inpatient_hcpcs)
summary(inpatient_diag) #confirm the inpatient data is available from 2008-01-01 to 2010-12-30 (discharge date)
###########################################################################################################################
#same as inpatient data, clean the outpatient data
outpatient<-readRDS('../dataset/outpatient.RData')
head(outpatient)
outpatient[,4:5]<-lapply(outpatient[,4:5],ymd)
outpatient<-outpatient[,c(1:2,4:5,13:28,31:76)]

outpatient_diag<-outpatient%>%select(c(1:14,21))%>%
                              gather(diag_cat,diag,starts_with('ICD9_DGNS'),na.rm=T)%>%
                              filter(diag!='')    

outpatient_proc<-outpatient%>%select(c(1:4,21,starts_with('ICD9_PRCDR')))%>%
                              gather(proc_cat,proc,starts_with('ICD9_PRCDR'),na.rm=T)%>%
                              filter(proc!='')
outpatient_hcpcs<-outpatient%>%select(c(1:4,21,starts_with('HCPCS')))%>%
                 gather(hcpcs_cat,hcpcs,starts_with('HCPCS'),na.rm=T)%>%
                 filter(hcpcs!='')
rm(outpatient)


##########################################################################################################################
#make sure there is enough memory in your computer before loading this data:

carrier<-readRDS('../dataset/carrier.RData')

#remove irrelevant variables:
carrier<-carrier[,c(1:12,39:51,130:142)]

carrier[,c('CLM_FROM_DT','CLM_THRU_DT')]<-lapply(carrier[,c('CLM_FROM_DT','CLM_THRU_DT')],ymd)
carrier_icd<-carrier%>%select(c(1:4,starts_with('ICD9'),starts_with('LINE_ICD9')))%>%
             gather(diag_cat,diag,starts_with('ICD9'),starts_with('LINE_ICD9'),na.rm=T)%>%
             filter(diag!='')
carrier_hcpcs<-carrier%>%select(c(1:4,starts_with('HCPCS')))%>%
               gather(hcpcs_cat,hcpcs,starts_with('HCPCS'),na.rm=T)%>%
               filter(hcpcs!='')


########################################################################################################
prescription<-readRDS('../dataset/prescription.RData')
prescription$SRVC_DT<-ymd(prescription$SRVC_DT)

#remove leading zeros in drug code:
prescription$PROD_SRVC_ID<-gsub('^0*','',prescription$PROD_SRVC_ID,perl = T)


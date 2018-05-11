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
beneficiary<-beneficiary[,c(1:23,33)]


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
carrier_a<-readRDS('../dataset/carrierA.RData')
carrier_b<-readRDS('../dataset/carrierB.RData')

#combine carrier datasets into one:
carrier<-rbind(carrier_a,carrier_b)
rm(carrier_a,carrier_b)

#remove irrelevant variables:
carrier<-carrier[,c(1:12,39:51,130:142)]



########################################################################################################
prescription<-readRDS('../dataset/prescription.RData')
prescription$SRVC_DT<-ymd(prescription$SRVC_DT)



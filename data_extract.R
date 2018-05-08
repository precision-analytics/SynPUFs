#download and import data:


library(rvest)
library(dplyr)
library(stringr)
library(data.table)

url<-'https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DESample01.html'

files<-url%>%read_html()%>%
             html_nodes('a')%>%
             html_attr('href')
files<-grep('Sample_1',files,value=T)
files_1<-paste0('http://www.cms.gov', files[grep('^/',files)])
files_2<-c(files_1,files[grep('^http',files)])

#create new folder and download files:
if(!(file.exists('../dataset'))) dir.create("../dataset")

filename<-str_extract(files,'(?<=DE1_0_).*')

for (i in seq_along(files_2)){
download.file(files_2[i], paste0('../dataset/',filename[i]))
unzip(paste0('../dataset/',filename[i]), exdir = "../dataset")
}


#read in csv files to R: (depending on your memory, you might consider loading files individually)
filename<-list.files('../dataset',pattern='csv$')
cms<-vector(mode='list',length=8)
for (i in seq_along(filename)){
  cms[[i]]<-fread(paste0('../dataset/',filename[i]),stringsAsFactors = F)
}

names(cms)<-str_extract(filename,'(?<=DE1_0_).*(?=.csv)')

#combine beneficiary data for year 2008 to 2010 into a single file:
#add year column to beneficairy date and combine:
cms[[1]]$year<-2008
cms[[7]]$year<-2009
cms[[8]]$year<-2010
beneficiary<-rbindlist(list(cms[[1]],cms[[7]],cms[[8]]))
#row number equivalent to summary table Table2. in Data user document


#combine carrier claims:
carrier_claim<-rbindlist(cms[[2]],cms[[3]])

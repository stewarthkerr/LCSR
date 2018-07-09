#Purpose: Convert demographics, exam, and follow-up data from the LCSP to a format
#         that can be uploaded to the ACR registry.
#Do: 
library(RCurl)
library(readr)
library(dplyr)
library(lubridate)
path<-paste("C:/Users/",Sys.getenv("USERNAME"),"/Desktop",sep='')

#Function that removes extra commas
xgsub <- function(x){
  x = gsub(',{1,}$','',x)
  x = gsub('^,{1,}','',x)
  x = gsub(',{2,}',',',x)
}

#Read in the three separate files via API
demo_api <- postForm(uri='https://redcap.duke.edu/redcap/api/',token='APITOKEN',content='report',format='csv',report_id='8889',rawOrLabel='raw',rawOrLabelHeaders='raw',exportCheckboxLabel='false',returnFormat='csv')
exam_api <- postForm(uri='https://redcap.duke.edu/redcap/api/',token='APITOKEN',content='report',format='csv',report_id='8891',rawOrLabel='raw',rawOrLabelHeaders='raw',exportCheckboxLabel='false',returnFormat='csv')
fu_api <- postForm(uri='https://redcap.duke.edu/redcap/api/',token='APITOKEN',content='report',format='csv',report_id='8890',rawOrLabel='raw',rawOrLabelHeaders='raw',exportCheckboxLabel='false',returnFormat='csv')
demo <- read.csv(textConnection(demo_api),stringsAsFactors = FALSE)
exam <- read.csv(textConnection(exam_api),stringsAsFactors = FALSE)
fu <- read.csv(textConnection(fu_api),stringsAsFactors = FALSE)
remove(demo_api,exam_api,fu_api)

#Remove REDCap specific variables and rename cor_date to exam_date
demo <- demo %>% select(-redcap_event_name)
exam <- exam %>% select(-redcap_event_name,-redcap_repeat_instrument,-redcap_repeat_instance)
fu <- fu %>% select(-redcap_event_name,-redcap_repeat_instrument,-redcap_repeat_instance) %>% rename(exam_date=cor_date)

#Merge so we have 1 row per unique MRN and exam date
redcap <- merge(demo,exam,by='redcap_record_id',all=TRUE) %>% merge(fu,by=c('redcap_record_id','exam_date'),all=TRUE)

#Convert dates to the correct format
redcap<-mutate(redcap,exam_date=strftime(ymd(exam_date),"%m/%d/%Y"),
               fu_date=strftime(ymd(fu_date),"%m/%d/%Y"),
               dob=strftime(ymd(dob),"%m/%d/%Y"),
               dod=strftime(ymd(dod),"%m/%d/%Y"))

#Perform changes within our redcap data frame
redcap = within(redcap, {
  
  #Placeholder for SSN
  refuse_ssn = 'Y'
  ssn = ''
  
  #Convert coding to registry appropriate format
  medicare_refuse = ifelse(medicare_refuse=='1','Y','N')
  dod_inv = ifelse(dod_inv=='0','N',ifelse(dod_inv=='1','Y',ifelse(dod_inv=='999','U','')))
  sex[sex==0] <- 'F'
  sex[sex==1] <- 'M'
  sex[sex==555] <- 'O'
  sex[sex==999] <- 'U'
  cessation[cessation==1] <- 'Y'
  cessation[cessation==0] <- 'N'
  cessation[cessation==999] <- 'U'
  shared_decision[shared_decision==1] <- 'Y'
  shared_decision[shared_decision==0] <- 'N'
  shared_decision[shared_decision==999] <- 'U'
  signs[signs==1] <- 'Y'
  signs[signs==0] <- 'N'
  lung_rads[lung_rads==4] <- '4A'
  lung_rads[lung_rads==5] <- '4B'
  lung_rads[lung_rads==6] <- '4X'
  recall[recall==1] <- 'I'
  recall[recall==2] <- 'N'
  recall[recall==3] <- 'M'
  recall[recall==4] <- 'E'
  recall[recall==5] <- 'OBa'
  lung_rads_s[lung_rads_s==1] <- 'Y'
  lung_rads_s[lung_rads_s==0] <- 'N'
  lung_rads_c[lung_rads_c==1] <- 'Y'
  lung_rads_c[lung_rads_c==0] <- 'N'
  lung_rads_c[lung_rads_c==999] <- 'U'
  radon[radon==1] <- 'Y'
  radon[radon==0] <- 'N'
  family_history[family_history==1] <- 'Y'
  family_history[family_history==0] <- 'N'
  family_history[family_history==999] <- 'U'
  family_history_other[family_history_other==1] <- 'Y'
  family_history_other[family_history_other==0] <- 'N'
  family_history_other[family_history_other==999] <- 'U'
  copd[copd==1] <- 'Y'
  copd[copd==0] <- 'N'
  pul_fib[pul_fib==1] <- 'Y'
  pul_fib[pul_fib==0] <- 'N'
  second_hand[second_hand==1] <- 'Y'
  second_hand[second_hand==0] <- 'N'
  second_hand[second_hand==999] <- 'U'
  overall_stage[overall_stage==1] <- 'IA'
  overall_stage[overall_stage==2] <- 'IB'
  overall_stage[overall_stage==3] <- 'IIA'
  overall_stage[overall_stage==4] <- 'IIB'
  overall_stage[overall_stage==5] <- 'IIIA'
  overall_stage[overall_stage==6] <- 'IIIB'
  overall_stage[overall_stage==7] <- 'IV'
  overall_stage[overall_stage==999] <- 'Unknown'
  t_status[t_status==1] <- 'Tx'
  t_status[t_status==2] <- 'T1a'
  t_status[t_status==3] <- 'T1b'
  t_status[t_status==4] <- 'T2a'
  t_status[t_status==5] <- 'T2b'
  t_status[t_status==6] <- 'T3'
  t_status[t_status==7] <- 'T4'
  n_status[n_status==4] <- 'NX'
  n_status[n_status==0] <- 'N0'
  n_status[n_status==1] <- 'N1'
  n_status[n_status==2] <- 'N2'
  n_status[n_status==3] <- 'N3'
  m_status[m_status==4] <- 'MX'
  m_status[m_status==0] <- 'M0'
  m_status[m_status==1] <- 'M1a'
  m_status[m_status==2] <- 'M1b'
  m_status[m_status==3] <- 'M1c'
  ct_manu[ct_manu==1] <- 'GE MEDICAL SYSTEMS'
  ct_manu[ct_manu==2] <- 'SIEMENS'
  siemens_model[siemens_model==1] <- 'Somatom Definition Edge'
  siemens_model[siemens_model==2] <- 'Somatom Definition Flash'
  siemens_model[siemens_model==3] <- 'Somatom Force'
  ge_model[ge_model==1] <- 'Discovery CT750 HD'
  ge_model[ge_model==2] <- 'LightSpeed VCT'
  ge_model[ge_model==3] <- 'LightSpeed XTRA'
  ge_model[ge_model==4] <- 'Revolution GSI'
  ge_model[ge_model==5] <- 'Revolution HD'
  ge_model[ge_model==6] <- 'Revolution CT'
  ge_model[ge_model==555] <- 'Revolution'
  
  #Collapse race into one variable
  for (i in c(1,2,3,4,5,6,10)){
    x <- paste("race___",i,sep='')
    z = ifelse(get(x)==1,i,'')
    assign(x,z)
  }
  race = ifelse(race___9=='1',9,paste(
    race___1,race___2,race___3,race___4,race___5,race___6,race___10,sep=','
  ))
  race = xgsub(race)

  #Collapse insurance into one variable
  for (i in 1:4){
    x <- paste("insurance___",i,sep='')
    z = ifelse(get(x)==1,i,'')
    assign(x,z)
  }
  insurance = ifelse(insurance___5=='1',5,paste(
    insurance___1,insurance___2,insurance___3,insurance___4,sep=','
  ))
  insurance = xgsub(insurance)

  #Collapse comorbid into 1 variable
  for (i in 0:8){
    x <- paste("comorbid___",i,sep='')
    z = ifelse(get(x)==1,i,'')
    assign(x,z)
  }
  comorbid = paste(
    comorbid___0,comorbid___1,comorbid___2,comorbid___3,comorbid___4,comorbid___5,comorbid___6,comorbid___7,comorbid___8,sep=','
  )
  comorbid = xgsub(comorbid) #Gets rid of extra commas

  #Collapse cancer_history into 1 variable
  for (i in 0:7){
    x <- paste("cancer_history___",i,sep='')
    z = ifelse(get(x)==1,i,'')
    assign(x,z)
  }
  cancer_history = paste(
    cancer_history___0,cancer_history___1,cancer_history___2,cancer_history___3,cancer_history___4,cancer_history___5,cancer_history___6,cancer_history___7,sep=','
  )
  cancer_history = xgsub(cancer_history) 

  #Determine which ordering provider NPI we should use (common or uncommon)
  order_npi = ifelse(com_order=='1',com_order_npi,order_npi)
  
  #Pick the ct_model variable based on the manufacturer, replace _ with space
  ct_model = ifelse(ct_manu=='GE MEDICAL SYSTEMS',ge_model,ifelse(ct_manu=='SIEMENS',siemens_model,''))

  #Collapse lung_rads_other into 1 variable
  for (i in 0:4){
    x <- paste("lung_rads_other___",i,sep='')
    z = ifelse(get(x)==1,i,'')
    assign(x,z)
  }
  lung_rads_other = paste(
    lung_rads_other___0,lung_rads_other___1,lung_rads_other___2,lung_rads_other___3,lung_rads_other___4,sep=','
  )
  lung_rads_other = xgsub(lung_rads_other) 

  #Collapse exposure into 1 variable
  for (i in 0:7){
    x <- paste("exposure___",i,sep='')
    z = ifelse(get(x)==1,i,'')
    assign(x,z)
  }
  exposure = paste(
    exposure___0,exposure___1,exposure___2,exposure___3,exposure___4,exposure___5,exposure___6,exposure___7,sep=','
  )
  exposure = xgsub(exposure) 

  #Collapse cancer_history_risk into 1 variable
  for (i in 0:4){
    x <- paste("cancer_history_risk___",i,sep='')
    z = ifelse(get(x)==1,i,'')
    assign(x,z)
  }
  cancer_history_risk = paste(
    cancer_history_risk___0,cancer_history_risk___1,cancer_history_risk___2,cancer_history_risk___3,cancer_history_risk___4,sep=','
  )
  cancer_history_risk = xgsub(cancer_history_risk) 
  
})

#Select the columns in the appropriate order
redcap<-select(redcap,redcap_record_id,pat_fname,pat_mname,pat_lname,mrn,refuse_ssn,ssn,medicare_refuse,medicare_id,dob,dod,dod_cause,dod_cause_other,cod,cod_other,dod_inv,sex,race,ethnicity,insurance,smoke,packs_year,quit,cessation,shared_decision,height,weight,comorbid,comorbid_other,cancer_history,cancer_history_other,radiologist,other_radiologist,order_npi,order_fname,order_lname,exam_date,signs,indication,modality,ct_manu,ct_model,ctdivol,dlp,tube_current_time,tube_voltage,scan_time,scan_volume,pitch,img_width,lung_rads,recall,lung_rads_s,lung_rads_other,lung_rads_mass,lung_rads_ild,lung_rads_ild_other,lung_rads_c,lung_cancer_prior,education,education_other,radon,exposure,cancer_history_risk,cancer_history_risk_other,family_history,family_history_other,copd,pul_fib,second_hand,fu_date,diagnostic,diagnostic_other,tissue,tissue_method,sample,sample_other,histology,histology_ns,histology_ns_other,stage,overall_stage,t_status,n_status,m_status)

#Filter off the records that need review
#These are records with other radiologist, no MRN, no exam_date
review <- filter(redcap,is.na(mrn)|is.na(exam_date)|radiologist=='555'|is.na(radiologist))
redcap <- filter(redcap,!is.na(mrn)&!is.na(exam_date)&radiologist!='555') %>% select(-other_radiologist)

#Get the time for the file timestamp
time<-gsub(' EDT$','',as.character(Sys.time()))
time<-gsub('-','',time)
time<-gsub(' ','-',time)
time<-gsub(':','',time)

#Filter based on the month we want
review <- filter(review,(year(mdy(exam_date))==2018 & ((month(mdy(exam_date))==4 ))))
redcap <- filter(redcap,(year(mdy(exam_date))==2018 & ((month(mdy(exam_date))==4 ))))

#Write the file for upload
write.csv(redcap,file=paste(path,'/lcsr_exam_',time,'.csv',sep=''),na='',row.names=FALSE)
write.table(redcap,file=paste(path,'/lcsr_exam_',time,'.txt',sep=''),sep='|',na='',quote=FALSE,row.names=FALSE,col.names=FALSE)

#Write the file for review
write.csv(review,file=paste(path,'/REGISTRY_REVIEW.csv',sep=''),na='',row.names=FALSE)
write.table(review,file=paste(path,'/REGISTRY_REVIEW.txt',sep=''),sep='|',na='',quote=FALSE,row.names=FALSE,col.names=FALSE)
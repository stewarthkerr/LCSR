#Purpose: Take data from Dynacad (which is in registry appropriate format) and convert to REDCap format

library(readr)
library(dplyr)
library(lubridate)
library(RCurl)
source('code/from_dynacad_functions.R') #These functions are used to translate from registry to REDCap

#Read in the dynacad file
dyna <- read_delim("output/To Registry/January 2018/lcsr_exam_20180308-155522.txt","|", escape_double = FALSE, col_names = FALSE,trim_ws = TRUE)

#Rename variables to REDCap names
colnames(dyna) <- c('redcap_record_id','pat_fname','pat_mname','pat_lname','mrn','refuse_ssn','ssn','medicare_refuse','medicare_id','dob','dod','dod_cause','dod_cause_other','cod','cod_other','dod_inv','sex','race','ethnicity','insurance','smoke','packs_year','quit','cessation','shared_decision','height','weight','comorbid','comorbid_other','cancer_history','cancer_history_other','radiologist','order_npi','order_fname','order_lname','exam_date','signs','indication','modality','ct_manu','ct_model','ctdivol','dlp','tube_current_time','tube_voltage','scan_time','scan_volume','pitch','img_width','lung_rads','recall','lung_rads_s','lung_rads_other','lung_rads_mass','lung_rads_ild','lung_rads_ild_other','lung_rads_c','lung_cancer_prior','education','education_other','radon','exposure','cancer_history_risk','cancer_history_risk_other','family_history','family_history_other','copd','pul_fib','second_hand','fu_date','diagnostic','diagnostic_other','tissue','tissue_method','sample','sample_other','histology','histology_ns','histology_ns_other','stage','overall_stage','t_status','n_status','m_status')

#Drop the fields not in REDCap
dyna <- select(dyna,-contains('ssn'))

#Pull in REDCap data so we can merge to populate REDCap record ID, event #, etc.
demo_api <- postForm(uri='https://redcap.duke.edu/redcap/api/',token='APITOKEN',content='report',format='csv',report_id='8889',rawOrLabel='raw',rawOrLabelHeaders='raw',exportCheckboxLabel='false',returnFormat='csv')
exam_api <- postForm(uri='https://redcap.duke.edu/redcap/api/',token='APITOKEN',content='report',format='csv',report_id='8891',rawOrLabel='raw',rawOrLabelHeaders='raw',exportCheckboxLabel='false',returnFormat='csv')
fu_api <- postForm(uri='https://redcap.duke.edu/redcap/api/',token='APITOKEN',content='report',format='csv',report_id='8890',rawOrLabel='raw',rawOrLabelHeaders='raw',exportCheckboxLabel='false',returnFormat='csv')
demo <- read.csv(textConnection(demo_api),stringsAsFactors = FALSE)
exam <- read.csv(textConnection(exam_api),stringsAsFactors = FALSE)
fu <- read.csv(textConnection(fu_api),stringsAsFactors = FALSE)
remove(demo_api,exam_api,fu_api)

#Populate REDCap record ID by matching on MRN - if no match, then new record
existing <- inner_join(select(dyna,-redcap_record_id),select(demo,mrn,redcap_record_id),by='mrn')
new <- anti_join(select(dyna,-redcap_record_id),select(demo,mrn,redcap_record_id),by='mrn')

#Do the REDCap translate
existing <- redcap_translate(existing)
new <- redcap_translate(new)
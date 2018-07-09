#Purpose: Take in data about the radiology scanners (provided via MaestroCare to DOCR team) and convert it the REDCap format
#Input: radiology data (radiology.csv) - needs to be located on desktop
#Output: RADIOLOGY_COMPLETE.csv - ready to go into REDCap
#		 RADIOLOGY_NEEDS_WORK.csv - there is something wrong with these records. Need manual review.

library(readr)
library(dplyr)
library(RCurl)

path <- paste("C:/Users/", Sys.getenv("USERNAME"), "/Desktop", sep = '')

#Read in the radiology dataset
radiology <- read_csv(paste(path, '/radiology.csv', sep = ''))

#Remove variables we don't care about and rename the variables we want, convert date to the REDCap format
radiology <-
  select(radiology, StudyDate, PatientID, Manufacturer, Model, CTDI, DLP) %>%
  rename(
    mrn = PatientID,
    exam_date = StudyDate,
    ct_manu = Manufacturer,
    ge_model = Model,
    ctdivol = CTDI,
    dlp = DLP
  ) %>%
  mutate(exam_date = as.Date(as.character(exam_date), "%Y%m%d"))

#Translate the data to what REDCap knows
radiology = within(radiology, {
  img_width <- '1'
  exam_baselineannual_complete <- '2'
  ct_manu = sub('GE MEDICAL SYSTEMS', 'GE', ct_manu)
  ge_model = if_else(grepl('CT750', ge_model), '1', ge_model)
  ge_model = if_else(grepl('VCT', ge_model), '2', ge_model)
  ge_model = if_else(grepl('XTRA', ge_model), '3', ge_model)
  ge_model = if_else(grepl('GSI', ge_model), '4', ge_model)
  ge_model = if_else(grepl('REVOLUTION HD', ge_model), '5', ge_model)
  ge_model = if_else(grepl('REVOLUTION CT', ge_model), '6', ge_model)
  ge_model = if_else(grepl('FLASH', ge_model), '2', ge_model)
  ge_model = if_else(grepl('FORCE', ge_model), '3', ge_model)
  ge_model = if_else(grepl('DEFINITION EDGE', ge_model), '1', ge_model)
  siemens_model = if_else(grepl('SIEMENS', ct_manu), ge_model, '')
  ge_model = if_else(grepl('SIEMENS', ct_manu), '', ge_model)
  ct_manu[ct_manu == 'GE'] <- '1'
  ct_manu[ct_manu == 'SIEMENS'] <- '2'
  exam_date = as.character(exam_date)
})
#Pull the REDCap data via API merging
exam_date_api <- postForm(uri='https://redcap.duke.edu/redcap/api/',token='APITOKEN',content='report',format='csv',report_id='8735',rawOrLabel='raw',rawOrLabelHeaders='raw',exportCheckboxLabel='false',returnFormat='csv')
mrn_api <- postForm(uri='https://redcap.duke.edu/redcap/api/',token='APITOKEN',content='report',format='csv',report_id='8758',rawOrLabel='raw',rawOrLabelHeaders='raw',exportCheckboxLabel='false',returnFormat='csv')
exam_date <- read.csv(textConnection(exam_date_api),stringsAsFactors = FALSE)
mrn <- read.csv(textConnection(mrn_api),stringsAsFactors = FALSE)
remove(exam_date_api,mrn_api)

#Perform the merges
radiology <-
  merge(radiology, mrn, by = 'mrn', all.x = TRUE) %>%
  select(-redcap_event_name) %>%
  merge(exam_date,
        by = c('redcap_record_id', 'exam_date'),
        all.x = TRUE) %>%
  select(
    redcap_record_id,
    redcap_event_name,
    redcap_repeat_instrument,
    redcap_repeat_instance,
    mrn,
    exam_date,
    exam_baselineannual_complete,
    ct_manu,
    ge_model,
    siemens_model,
    ctdivol,
    dlp,
    img_width
  )

#These records don't exist in REDCap either because the patient or exam hasn't been entered
need_work <-
  filter(radiology,
         is.na(redcap_record_id) | is.na(redcap_repeat_instance))

radiology <-
  filter(radiology,!is.na(redcap_record_id) &
           !is.na(redcap_repeat_instance)) %>% select(-mrn)

write.csv(
  need_work,
  file = paste(path, '/RADIOLOGY_NEEDS_WORK.csv', sep = ''),
  na = '',
  row.names = FALSE
)
write.csv(
  radiology,
  file = paste(path, '/RADIOLOGY_COMPLETE.csv', sep = ''),
  na = '',
  row.names = FALSE
)
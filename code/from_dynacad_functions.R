#Purpose: Create some functions that we need to translate the data from Dynacad (registry format) to REDCap format

#Translate registry format to REDCap format
redcap_translate <- function(x){
  x = within(x, {
  
  #Define some empty columns we will fill
  siemens_model=''
  ge_model=''
    
  #Convert coding to REDCap appropriate format
  medicare_refuse = ifelse(medicare_refuse=='Y','1','0')
  dod_inv = ifelse(dod_inv=='N','0',ifelse(dod_inv=='Y','1',ifelse(dod_inv=='U','999','')))
  sex[sex=='F'] <- 0
  sex[sex=='M'] <- 1
  sex[sex=='O'] <- 555
  sex[sex=='U'] <- 999
  cessation[cessation=='Y'] <- 1
  cessation[cessation=='N'] <- 0
  cessation[cessation=='U'] <- 999
  shared_decision[shared_decision=='Y'] <- 1
  shared_decision[shared_decision=='N'] <- 0
  shared_decision[shared_decision=='U'] <- 999
  signs[signs=='Y'] <- 1
  signs[signs=='N'] <- 0
  lung_rads[lung_rads=='4A'] <- 4
  lung_rads[lung_rads=='4B'] <- 5
  lung_rads[lung_rads=='4x'] <- 6
  recall[recall=='I'] <- 1
  recall[recall=='N'] <- 2
  recall[recall=='M'] <- 3
  recall[recall=='E'] <- 4
  recall[recall=='OBa'] <- 5
  lung_rads_s[lung_rads_s=='Y'] <- 1
  lung_rads_s[lung_rads_s=='N'] <- 0
  lung_rads_c[lung_rads_c=='Y'] <- 1
  lung_rads_c[lung_rads_c=='N'] <- 0
  lung_rads_c[lung_rads_c=='U'] <- 999
  radon[radon=='Y'] <- 1
  radon[radon=='N'] <- 0
  family_history[family_history=='Y'] <- 1
  family_history[family_history=='N'] <- 0
  family_history[family_history=='U'] <- 999
  family_history_other[family_history_other=='Y'] <- 1
  family_history_other[family_history_other=='N'] <- 0
  family_history_other[family_history_other=='U'] <- 999
  copd[copd=='Y'] <- 1
  copd[copd=='N'] <- 0
  pul_fib[pul_fib=='Y'] <- 1
  pul_fib[pul_fib=='N'] <- 0
  second_hand[second_hand=='Y'] <- 1
  second_hand[second_hand=='N'] <- 0
  second_hand[second_hand=='U'] <- 999
  overall_stage[overall_stage=='IA'] <- 1
  overall_stage[overall_stage=='IB'] <- 2
  overall_stage[overall_stage=='IIA'] <- 3
  overall_stage[overall_stage=='IIB'] <- 4
  overall_stage[overall_stage=='IIIA'] <- 5
  overall_stage[overall_stage=='IIIB'] <- 6
  overall_stage[overall_stage=='IV'] <- 7
  overall_stage[overall_stage=='Unknown'] <- 999
  t_status[t_status=='Tx'] <- 1
  t_status[t_status=='T1a'] <- 2
  t_status[t_status=='T1b'] <- 3
  t_status[t_status=='T2a'] <- 4
  t_status[t_status=='T2b'] <- 5
  t_status[t_status=='T3'] <- 6
  t_status[t_status=='T4'] <- 7
  n_status[n_status=='NX'] <- 4
  n_status[n_status=='N0'] <- 0
  n_status[n_status=='N1'] <- 1
  n_status[n_status=='N2'] <- 2
  n_status[n_status=='N3'] <- 3
  m_status[m_status=='MX'] <- 4
  m_status[m_status=='M0'] <- 0
  m_status[m_status=='M1a'] <- 1
  m_status[m_status=='M1b'] <- 2
  m_status[m_status=='M1c'] <- 3
  ct_manu[ct_manu=='GE MEDICAL SYSTEMS'] <- 1
  ct_manu[ct_manu=='SIEMENS'] <- 2
  siemens_model[ct_model=='Somatom Definition Edge'] <- 1
  siemens_model[ct_model=='Somatom Definition Flash'] <- 2
  siemens_model[ct_model=='SOMATOM Force'] <- 3
  ge_model[ct_model=='Discovery CT750 HD'] <- 1
  ge_model[ct_model=='LightSpeed VCT'] <- 2
  ge_model[ct_model=='LightSpeed XTRA'] <- 3
  ge_model[ct_model=='Revolution GSI'] <- 4
  ge_model[ct_model=='Revolution HD'] <- 5
  ge_model[ct_model=='Revolution CT'] <- 6
  ge_model[ct_model=='Revolution'] <- 555}) #We've closed the within
  
  ###THIS IS THE CURRENT LINE OF WORK###
  #Expand race,insurance,comorbid,cancer_history into multiple variables
  x <- mutate(x,
              race___1 = ifelse(grepl('1',race),1,0),
              race___2 = ifelse(grepl('2',race),1,0),
              race___3 = ifelse(grepl('3',race),1,0),
              race___4 = ifelse(grepl('4',race),1,0),
              race___5 = ifelse(grepl('5',race),1,0),
              race___6 = ifelse(grepl('6',race),1,0),
              race___9 = ifelse(grepl('9',race),1,0),
              race___10 = ifelse(grepl('10',race),1,0),
              insurance___1 = ifelse(grepl('1',race),1,0),
              insurance___2 = ifelse(grepl('2',race),1,0),
              insurance___3 = ifelse(grepl('3',race),1,0),
              insurance___4 = ifelse(grepl('4',race),1,0),
              insurance___5 = ifelse(grepl('5',race),1,0),
              comorbid___1 = ifelse(grepl('1',race),1,0),
              comorbid___2 = ifelse(grepl('2',race),1,0),
              comorbid___3 = ifelse(grepl('3',race),1,0),
              comorbid___4 = ifelse(grepl('4',race),1,0),
              comorbid___5 = ifelse(grepl('5',race),1,0),
              comorbid___6 = ifelse(grepl('6',race),1,0),
              comorbid___7 = ifelse(grepl('7',race),1,0),
              comorbid___8 = ifelse(grepl('8',race),1,0),
              cancer_history___1 = ifelse(grepl('1',race),1,0),
              cancer_history___2 = ifelse(grepl('2',race),1,0),
              cancer_history___3 = ifelse(grepl('3',race),1,0),
              cancer_history___4 = ifelse(grepl('4',race),1,0),
              cancer_history___5 = ifelse(grepl('5',race),1,0),
              cancer_history___6 = ifelse(grepl('6',race),1,0),
              cancer_history___7 = ifelse(grepl('7',race),1,0),
              lung_rads_other___0 = ifelse(grepl('0',race),1,0),
              lung_rads_other___1 = ifelse(grepl('1',race),1,0),
              lung_rads_other___2 = ifelse(grepl('2',race),1,0),
              lung_rads_other___3 = ifelse(grepl('3',race),1,0),
              lung_rads_other___4 = ifelse(grepl('4',race),1,0),
              exposure___0 = ifelse(grepl('0',race),1,0),
              exposure___1 = ifelse(grepl('1',race),1,0),
              exposure___2 = ifelse(grepl('2',race),1,0),
              exposure___3 = ifelse(grepl('3',race),1,0),
              exposure___4 = ifelse(grepl('4',race),1,0),
              exposure___5 = ifelse(grepl('5',race),1,0),
              exposure___6 = ifelse(grepl('6',race),1,0),
              exposure___7 = ifelse(grepl('7',race),1,0),
              cancer_history_risk___0 = ifelse(grepl('0',race),1,0),
              cancer_history_risk___1 = ifelse(grepl('1',race),1,0),
              cancer_history_risk___2 = ifelse(grepl('2',race),1,0),
              cancer_history_risk___3 = ifelse(grepl('3',race),1,0),
              cancer_history_risk___4 = ifelse(grepl('4',race),1,0)) %>%
      select(-ct_model,-race,-insurance,-comorbid,-cancer_history)
  
  return(x)}

  #Determine which ordering provider NPI we should use (common or uncommon)
  #order_npi = ifelse(com_order=='1',com_order_npi,order_npi)

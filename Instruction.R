library(arsenal)
library(data.table)
library(tidyselect)
library(tidyverse)
library(stringr)
# Get the data
df<-read.csv("consulting project.csv",header=TRUE) 

## get the target diagnosis code
diagnosistar<-c('S02.85', 'S02.3', 'S02.12', 'S02.84', 'S02.83','802.6','802.7') # Target Diagnosis code you want


## get the target procedure code
proceduretar<-c('"16.0"', "16.01", '16.09', '16.8', '16.81', '16.89','0N5P', '0N5Q',        
                '0N8P', '0N8Q', '0N9P', '0N9Q', '0NBP', '0NBQ', '0NCP', '0NCQ', '0NDP', 
                '0NDQ', '0NHP', '0NHQ', '0NNP', '0NNQ', '0NQP', '0NQN', '0NRP', '0NRQ',
                '0NSP', '0NSQ', '0NTP', '0NTQ', '0NUP', '0NUQ')

## Function for finding data with specific target code
get_candi<-function(target,df){                     
  tmp=startsWith(c(as.matrix(df)), target)
  #col_name=colnames(df)
  tmp2=data.frame(matrix(tmp,ncol=369))
  tmp_index=apply(tmp2,1,sum,na.rm = T)>0
  re=df[tmp_index,]
  return(re)
}
# Get the total number of patient
Num_of_patient<-nrow(df)


# Get the result and number for patient with target diagnosis code
l<-lapply(diagnosistar,get_candi,df=df)
Onlywithdiagnosis<-distinct(bind_rows(l)) 
Num_of_patients_with_diagnosis<-nrow(Onlywithdiagnosis)


# Get the result and number for with both target diagnosis code and procedure code
w<-lapply(proceduretar,get_candi,df=Onlywithdiagnosis)
Bothdiagnosis_and_procedure <-distinct(bind_rows(w)) 
Num_of_patients_with_diagnosis_and_procedure<-nrow(Bothdiagnosis_and_procedure)


# Get the result and number of patient with target procedure code
v<-lapply(proceduretar,get_candi,df=df)
Onlywithprocedure<-distinct(bind_rows(v)) 
Num_of_patients_with_procedure<-nrow(Onlywithprocedure)

# Get the number of patient with target procedure code but no target diagnosis code
Num_of_patient_with_procedure_no_daignosis<-Num_of_patients_with_procedure - Num_of_patients_with_diagnosis_and_procedure

# Get the number of patient with target diagnosis but not target procedure
Num_of_patient_with_diagnosis_no_procedure <- Num_of_patients_with_diagnosis - Num_of_patients_with_diagnosis_and_procedure

# Get the number of patient with no target diagnosis
Num_of_patient_with_no_target_diagnosis <- Num_of_patient - Num_of_patients_with_diagnosis

# Get the number of patient with no target diagnosis and no target procedure
Num_of_patient_with_no_target_diagnosis_no_procedure <- Num_of_patient_with_no_target_diagnosis - Num_of_patient_with_procedure_no_daignosis

# Get the number of patient with no target procedure
Num_of_patient_with_no_procedure<- Num_of_patient - Num_of_patients_with_procedure

# Create contingency table 

data.frame('patient with target procedure' = c(2,5,7),
           'patient with no target procedure' = c(53,258,311),
           'total '=c(55,263,318),row.names = c('patient with target diagnosis',
                                                'patient with no target diagnosis','total'))

# t-test part
2/55

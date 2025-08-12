
####Exercise####

#1.Check Cholesterol level (using if)

#using the if function

cholesterol<-230
#writing the if statement to check if cholesterol is greater than 240
if(cholesterol>230){
  print("High Cholesterol")
}  


#2.Blood Pressure Status (using if...else)

#using if else condition

systolic_bp<- 130
  if(systolic_bp<120){
    print("Blood Pressure is normal")
  }else{
    if(systolic_bp>120){
      print("Blood Pressure is high")
    }
  }


#3.Automating data with for loop
patient_info<-read.csv(file.choose())
View(patient_info) 

#creating a copy
cleaned_data<-patient_info
View(cleaned_data)
str(patient_info)

#for patient_info 
factor_cols<-c("gender","diagnosis","smoker")

#using the for loop to convert to factor

for (col in factor_cols) {
 patient_info[[col]]<-as.factor(patient_info[[col]])
}
str(patient_info)


#for cleaned data

for (col in factor_cols) {
  cleaned_data[[col]]<-as.factor(cleaned_data[[col]])
} 
str(cleaned_data)


#4.Converting Factors to numeric factors in cleaned_data

#for gender

gender_num<-c("gender")
for (col in gender_num) {
 cleaned_data[[col]]<-ifelse(cleaned_data$gender=="Female",1,0)
  
}

#for diagnosis

diagnosis_num<-c("diagnosis")
for (col in diagnosis_num) {
  cleaned_data[[col]]<-ifelse(cleaned_data$diagnosis=="Normal",1,0)
  
}

#for smoker

smoker_num<-c("smoker")
for (col in smoker_num) {
  cleaned_data[[col]]<-ifelse(cleaned_data$smoker=="Yes",1,0)
  
}

#5.Verification
#comparing the original data with the cleaned_data
str(patient_info)
str(cleaned_data)

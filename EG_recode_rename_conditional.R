

#How to change value of one variable based on value of another

#Format
#    df$var2bechanged [df$varwcondition OPERATOR condition] <- value to be entered

#EG
#    eia_svc_ter2015$state_fp[eia_svc_ter2015$CountyState=="Manu AAS"] <-"60"

#In words
#In eia_svc dataset $ for variable state_fp 
#[if var CountyState in eia set == "Manu AAS"] 
#<- state_fp becomes "60"


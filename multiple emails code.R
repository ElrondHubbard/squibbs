library(dplyr)
library(reshape2)

multiple_emails_dat<-read.csv("C:\\Users\\myoshida\\Desktop\\multiple_emails.csv", header=TRUE)
biglist<-read.csv("C:\\Users\\myoshida\\Desktop\\multiple emails\\all_email_active_type_l_addr_181008.csv", header=TRUE)

#Creating a ID for each unique ID_NUMBER
dat<-multiple_emails_dat
dat$entity <- cumsum(!duplicated(dat$ID_NUMBER))


###Creating a subset based on having an LUC email###


dat.wide<-reshape(transform(dat, time=ave(entity,entity,FUN=seq_along)), 
              idvar="entity", direction="wide", sep="")

luc.emails<-dat.wide %>% filter(LUC_ADDR1==1|LUC_ADDR2==1)

#(CHECKING)We have one less row in the wide data frame b/c there is an ID with two LUC emails
#ID number 566982 

test.luc<-subset(dat,LUC_ADDR==1)

test.twoemails<-mutate(luc.emails, two_LUC=ifelse(LUC_ADDR1==1&LUC_ADDR2==1,1,0))

test.twoemails[which(test.twoemails$two_LUC==1),]

#Structure changing
library(splus2R)

luc.emails$EMAIL_ADDRESS1<-lowerCase(luc.emails$EMAIL_ADDRESS1)
luc.emails$EMAIL_ADDRESS2<-lowerCase(luc.emails$EMAIL_ADDRESS2)

luc.emails$EMAIL_ADDRESS1<-as.character(luc.emails$EMAIL_ADDRESS1)
luc.emails$EMAIL_ADDRESS2<-as.character(luc.emails$EMAIL_ADDRESS2)
luc.emails$EMAIL_TYPE_CODE1<-as.character(luc.emails$EMAIL_TYPE_CODE1)
luc.emails$EMAIL_TYPE_CODE2<-as.character(luc.emails$EMAIL_TYPE_CODE2)

#If the e-mail is already in biglist, create a delete flag
biglist$EMAIL_ADDRESS<-as.character(biglist$EMAIL_ADDRESS)


luc.emails<-mutate(luc.emails, luc_already1=
                     ifelse(luc.emails$EMAIL_ADDRESS1 %in% biglist$EMAIL_ADDRESS,1,0))

luc.emails<-mutate(luc.emails, luc_already2=
                     ifelse(luc.emails$EMAIL_ADDRESS2 %in% biglist$EMAIL_ADDRESS,1,0))

luc.emails$final_EMAIL_TYPE_CODE1<-
  ifelse(luc.emails$luc_already1==1, "DEL", luc.emails$EMAIL_TYPE_CODE1)

luc.emails$final_EMAIL_TYPE_CODE2<-
  ifelse(luc.emails$luc_already2==1, "DEL", luc.emails$EMAIL_TYPE_CODE1)


#If not in biglist and LUC email was automated in, change that email code to L

luc.emails$email1_LUC<- ifelse(grepl("luc", luc.emails$EMAIL_ADDRESS1), 1, 0)
luc.emails$email2_LUC<- ifelse(grepl("luc", luc.emails$EMAIL_ADDRESS2), 1, 0)

luc.emails$OPERATOR_NAME1<-as.character(luc.emails$OPERATOR_NAME1)
luc.emails$OPERATOR_NAME2<-as.character(luc.emails$OPERATOR_NAME2)

luc.emails$operator.luc1<-ifelse(luc.emails$luc_already1==0&luc.emails$luc_already2==0&
  grepl("LU", luc.emails$OPERATOR_NAME1), 1, 0)

luc.emails$operator.luc2<-ifelse(luc.emails$luc_already1==0&luc.emails$luc_already2==0&
                                   grepl("LU", luc.emails$OPERATOR_NAME2), 1, 0)
  
luc.emails$final_EMAIL_TYPE_CODE1<-
  ifelse(luc.emails$operator.luc1==1&luc.emails$email1_LUC==1,
         "L", luc.emails$final_EMAIL_TYPE_CODE1)

luc.emails$final_EMAIL_TYPE_CODE2<-
  ifelse(luc.emails$operator.luc2==1&luc.emails$email2_LUC==1,
         "L", luc.emails$final_EMAIL_TYPE_CODE2)








#After looking through excel file (n=22 where neither of emails is in LUC email database
# & LUC email was not added in through LUC operator), ID numbers
# 302406, 373620, 475922 will keep their LUC emails as preferred, others will have other 
# email as preferred

luc.emails$final_EMAIL_TYPE_CODE1<-
  ifelse(luc.emails$luc_already1==0&luc.emails$luc_already2==0
         &luc.emails$email1_LUC==1
         &luc.emails$ID_NUMBER1!=302406
         &luc.emails$ID_NUMBER1!=373620
         &luc.emails$ID_NUMBER1!=475922,
         "L", luc.emails$final_EMAIL_TYPE_CODE1)

luc.emails$final_EMAIL_TYPE_CODE2<-
  ifelse(luc.emails$luc_already1==0&luc.emails$luc_already2==0
         &luc.emails$email2_LUC==1
         &luc.emails$ID_NUMBER2!=302406
         &luc.emails$ID_NUMBER2!=373620
         &luc.emails$ID_NUMBER2!=475922,
         "L", luc.emails$final_EMAIL_TYPE_CODE2)

luc.emails$final_EMAIL_TYPE_CODE1<-
  ifelse(luc.emails$luc_already1==0&luc.emails$luc_already2==0
         &luc.emails$email1_LUC==0
         &(luc.emails$ID_NUMBER1==302406
         |luc.emails$ID_NUMBER1==373620
         |luc.emails$ID_NUMBER1==475922),
         "L", luc.emails$final_EMAIL_TYPE_CODE1)


luc.emails$final_EMAIL_TYPE_CODE2<-
  ifelse(luc.emails$luc_already1==0&luc.emails$luc_already2==0
         &luc.emails$email2_LUC==0
         &(luc.emails$ID_NUMBER2==302406
         |luc.emails$ID_NUMBER2==373620
         |luc.emails$ID_NUMBER2==475922),
         "L", luc.emails$final_EMAIL_TYPE_CODE2)

write.csv(luc.emails, file="emailremaindersv4.csv")








####NOW LOOKING AT INSTANCES WHERE NEITHER EMAIL IS AN LUC EMAIL####

non.emails<-dat.wide %>% filter(LUC_ADDR1==0&LUC_ADDR2==0)

non.emails$EMAIL_ADDRESS1<-lowerCase(non.emails$EMAIL_ADDRESS1)
non.emails$EMAIL_ADDRESS2<-lowerCase(non.emails$EMAIL_ADDRESS2)

non.emails$EMAIL_ADDRESS1<-as.character(non.emails$EMAIL_ADDRESS1)
non.emails$EMAIL_ADDRESS2<-as.character(non.emails$EMAIL_ADDRESS2)
non.emails$EMAIL_TYPE_CODE1<-as.character(non.emails$EMAIL_TYPE_CODE1)
non.emails$EMAIL_TYPE_CODE2<-as.character(non.emails$EMAIL_TYPE_CODE2)

#write.csv(non.emails,file="nonlucemails.csv")

#Dates

non.emails$DATE_ADDED1<-as.Date(as.character(non.emails$DATE_ADDED1), format="%d-%b-%y")
non.emails$DATE_ADDED2<-as.Date(as.character(non.emails$DATE_ADDED2),format="%d-%b-%y")
non.emails$DATE_MODIFIED1<-as.Date(as.character(non.emails$DATE_MODIFIED1),format="%d-%b-%y")
non.emails$DATE_MODIFIED2<-as.Date(as.character(non.emails$DATE_MODIFIED2), format="%d-%b-%y")

ref_date<-as.Date(as.character("2018-10-08"), format="%Y-%m-%d")

non.emails$days_since_add1<-ref_date-non.emails$DATE_ADDED1
non.emails$days_since_add2<-ref_date-non.emails$DATE_ADDED2
non.emails$days_since_mod1<-ref_date-non.emails$DATE_MODIFIED1
non.emails$days_since_mod2<-ref_date-non.emails$DATE_MODIFIED2

#Gmail indicator 

non.emails$email1_gmail<- ifelse(grepl("gmail", non.emails$EMAIL_ADDRESS1), 1, 0)
non.emails$email2_gmail<- ifelse(grepl("gmail", non.emails$EMAIL_ADDRESS2), 1, 0)

twogmails<-non.emails[which(non.emails$email1_gmail==1&non.emails$email2_gmail==1),]
no.gmails<-non.emails[which(non.emails$email1_gmail==0&non.emails$email2_gmail==0),]

new_non.emails<-anti_join(non.emails, twogmails)
new_non.emails<-anti_join(new_non.emails,no.gmails)

#write.csv(twogmails,file="twogmails.csv")
#write.csv(no.gmails,file="nogmails.csv")

#For the IDs that have 1 gmail, take the non-gmail, if it is older than gmail, make it O
new_non.emails$final_EMAIL_TYPE_CODE1<-
  ifelse(new_non.emails$email1_gmail==0
         &new_non.emails$days_since_add1>new_non.emails$days_since_add2,
         "O", new_non.emails$EMAIL_TYPE_CODE1)

new_non.emails$final_EMAIL_TYPE_CODE2<-
  ifelse(new_non.emails$email2_gmail==0
         &new_non.emails$days_since_add2>new_non.emails$days_since_add1,
         "O", new_non.emails$EMAIL_TYPE_CODE2)

nrow(new_non.emails[which(new_non.emails$final_EMAIL_TYPE_CODE1=="E"&
                            new_non.emails$final_EMAIL_TYPE_CODE2=="E" ),])

#Still leaves us with 48 IDs with two pref emails, filter out bad domains

new_non.emails$bad_email1<-ifelse(grepl("sbcgl|comcast|charter|cox|lumc.edu", new_non.emails$EMAIL_ADDRESS1), 1, 0)
new_non.emails$bad_email2<-ifelse(grepl("sbcgl|comcast|charter|cox|lumc.edu", new_non.emails$EMAIL_ADDRESS2), 1, 0)

new_non.emails$final_EMAIL_TYPE_CODE1<-
  ifelse(new_non.emails$bad_email1==1,
         "O", new_non.emails$final_EMAIL_TYPE_CODE1)

new_non.emails$final_EMAIL_TYPE_CODE2<-
  ifelse(new_non.emails$bad_email2==1,
         "O", new_non.emails$final_EMAIL_TYPE_CODE2)

#Still 36 unfixed
#Looked through excel sheet, found exceptions where gmail should not be preferred 
new_non.emails_unfixed<-new_non.emails[which(new_non.emails$final_EMAIL_TYPE_CODE1=="E"&
                            new_non.emails$final_EMAIL_TYPE_CODE2=="E" ),]

new_non.emails<-anti_join(new_non.emails,new_non.emails_unfixed)

new_non.emails_unfixed$final_EMAIL_TYPE_CODE1<-
  ifelse(new_non.emails_unfixed$email1_gmail==0&
           new_non.emails_unfixed$ID_NUMBER1!=84208&
           new_non.emails_unfixed$ID_NUMBER1!=394436&
           new_non.emails_unfixed$ID_NUMBER1!=397358,
         "O", new_non.emails_unfixed$final_EMAIL_TYPE_CODE1)

new_non.emails_unfixed$final_EMAIL_TYPE_CODE2<-
  ifelse(new_non.emails_unfixed$email2_gmail==0&
           new_non.emails_unfixed$ID_NUMBER2!=84208&
           new_non.emails_unfixed$ID_NUMBER2!=394436&
           new_non.emails_unfixed$ID_NUMBER2!=397358,
         "O", new_non.emails_unfixed$final_EMAIL_TYPE_CODE2)

new_non.emails_unfixed$final_EMAIL_TYPE_CODE1<-
  ifelse(new_non.emails_unfixed$email1_gmail==1&
           (new_non.emails_unfixed$ID_NUMBER1==84208|
           new_non.emails_unfixed$ID_NUMBER1==394436|
           new_non.emails_unfixed$ID_NUMBER1==397358),
         "O", new_non.emails_unfixed$final_EMAIL_TYPE_CODE1)

new_non.emails_unfixed$final_EMAIL_TYPE_CODE2<-
  ifelse(new_non.emails_unfixed$email2_gmail==1&
           (new_non.emails_unfixed$ID_NUMBER2==84208|
           new_non.emails_unfixed$ID_NUMBER2==394436|
           new_non.emails_unfixed$ID_NUMBER2==397358),
         "O", new_non.emails_unfixed$final_EMAIL_TYPE_CODE2)






                                  
#write.csv(new_non.emails,file="emails_1gmail_fixed.csv")
#write.csv(new_non.emails_unfixed,file="emails_1gmail_unfixedv2.csv")


#For IDs with two gmails, go with the one more recently added

twogmails$final_EMAIL_TYPE_CODE1<-
  ifelse(twogmails$days_since_add1>twogmails$days_since_add2,
         "O", twogmails$EMAIL_TYPE_CODE2)

twogmails$final_EMAIL_TYPE_CODE2<-
  ifelse(twogmails$days_since_add2>twogmails$days_since_add1,
         "O", twogmails$EMAIL_TYPE_CODE1)

#For IDs with no gmails, manually look through n=50 cases, add final email type codes

no.gmails.updated<-read.csv("C:\\Users\\myoshida\\Documents\\nogmails_manupdated.csv", header=TRUE)


#Joining everything back together


new_non.emails$days_since_add1<-as.integer(new_non.emails$days_since_add1)
new_non.emails$days_since_add2<-as.integer(new_non.emails$days_since_add2)
new_non.emails$days_since_mod1<-as.integer(new_non.emails$days_since_mod1)
new_non.emails$days_since_mod2<-as.integer(new_non.emails$days_since_mod2)

new_non.emails_unfixed$days_since_add1<-as.integer(new_non.emails_unfixed$days_since_add1)
new_non.emails_unfixed$days_since_add2<-as.integer(new_non.emails_unfixed$days_since_add2)
new_non.emails_unfixed$days_since_mod1<-as.integer(new_non.emails_unfixed$days_since_mod1)
new_non.emails_unfixed$days_since_mod2<-as.integer(new_non.emails_unfixed$days_since_mod2)

twogmails$days_since_add1<-as.integer(twogmails$days_since_add1)
twogmails$days_since_add2<-as.integer(twogmails$days_since_add2)
twogmails$days_since_mod1<-as.integer(twogmails$days_since_mod1)
twogmails$days_since_mod2<-as.integer(twogmails$days_since_mod2)

no.gmails.updated$DATE_ADDED1<-as.Date(as.character(no.gmails.updated$DATE_ADDED1), format="%d-%b-%y")
no.gmails.updated$DATE_ADDED2<-as.Date(as.character(no.gmails.updated$DATE_ADDED2),format="%d-%b-%y")
no.gmails.updated$DATE_MODIFIED1<-as.Date(as.character(no.gmails.updated$DATE_MODIFIED1),format="%d-%b-%y")
no.gmails.updated$DATE_MODIFIED2<-as.Date(as.character(no.gmails.updated$DATE_MODIFIED2), format="%d-%b-%y")

luc.emails$DATE_ADDED1<-as.Date(as.character(luc.emails$DATE_ADDED1), format="%d-%b-%y")
luc.emails$DATE_ADDED2<-as.Date(as.character(luc.emails$DATE_ADDED2), format="%d-%b-%y")
luc.emails$DATE_MODIFIED1<-as.Date(as.character(luc.emails$DATE_MODIFIED1),format="%d-%b-%y")
luc.emails$DATE_MODIFIED2<-as.Date(as.character(luc.emails$DATE_MODIFIED2),format="%d-%b-%y")

multiple_emails_1<-full_join(new_non.emails,new_non.emails_unfixed)
multiple_emails_2<-full_join(multiple_emails_1,no.gmails.updated)
multiple_emails_3<-full_join(multiple_emails_2,twogmails)
multiple_emails_final<-full_join(multiple_emails_3,luc.emails)


final_dat<-multiple_emails_final%>% select(ID_NUMBER1, ID_NUMBER2, EMAIL_ADDRESS1,EMAIL_ADDRESS2,
                                final_EMAIL_TYPE_CODE1, final_EMAIL_TYPE_CODE2, luc_already1, luc_already2)


#Person with two luc e-mails, as found before---change one e-mail 

final_dat[which(final_dat$EMAIL_ADDRESS1==final_dat$EMAIL_ADDRESS2),]

final_dat$final_EMAIL_TYPE_CODE1<-
  ifelse(final_dat$ID_NUMBER1==566982,
         "E", final_dat$final_EMAIL_TYPE_CODE1)

#Quick check

final_dat[which(final_dat$final_EMAIL_TYPE_CODE1=="E"&final_dat$final_EMAIL_TYPE_CODE2=="E"),]
final_dat[which(final_dat$final_EMAIL_TYPE_CODE1=="O"&final_dat$final_EMAIL_TYPE_CODE2=="O"),]
final_dat[which(final_dat$final_EMAIL_TYPE_CODE1=="L"&final_dat$final_EMAIL_TYPE_CODE2=="L"),]

#We still have one ID (313247) where there are 2 type L emails, neither of which are found in Advance already. changed the luc.edu one to preferred

final_dat$final_EMAIL_TYPE_CODE2<-
  ifelse(final_dat$ID_NUMBER1== 313247,
         "E", final_dat$final_EMAIL_TYPE_CODE2)

final_dat[which(final_dat$ID_NUMBER1==313247),]




#write.csv(multiple_emails_final, file="multiple_emails_final.csv")

final_dat1<-reshape(final_dat, 
                    direction="long",varying=list(c("ID_NUMBER1","ID_NUMBER2"),
                                            c("final_EMAIL_TYPE_CODE1", "final_EMAIL_TYPE_CODE2"),
                                            c("luc_already1","luc_already2"),
                                            c("EMAIL_ADDRESS1","EMAIL_ADDRESS2")),
                                            v.names=c("ID_NUMBER","final_EMAIL_TYPE_CODE", "LUC_ALREADY", "EMAIL_ADDRESS"))
                           
final_dat1<-final_dat1[order(final_dat1$ID_NUMBER),]


                 
final_dat2<-final_dat1%>%select(ID_NUMBER,final_EMAIL_TYPE_CODE, LUC_ALREADY, EMAIL_ADDRESS) 

final_dat2$LUC_ALREADY[is.na(final_dat2$LUC_ALREADY)]<- 0

write.csv(final_dat2,file="multiple_emails_updated_MY.csv")

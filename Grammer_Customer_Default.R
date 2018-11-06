###########################
#Gramener group case study
##########################

#setwd("C:/Users/OM/Downloads/UIIITB Gramener Case study/loan")
library(dplyr)
library(tidyr)
library(stringr)
library(caret)
library(reshape2)
library(ggplot2)
library(VIM)

loan=read.csv("loan.csv",stringsAsFactors = F,na.strings = c("","NA"," ","n/a"))

View(loan)
str(loan)
summary(loan)

#################################################
#Data Cleaning
################################################

#####
#treating NA
####
#removing columns with more than 50% na

x=colSums(is.na(loan))
x
y=names(x[x<.5*nrow(loan)])

loan=loan[,y]

colSums(is.na(loan))

sum(is.na(loan))

##################   checking duplicates ##########################

#no duplicate records 
table(duplicated(loan))
#39717 

summary(loan)
str(loan)
####################    removing unnecessary columns  ###############################

#desc its the description with raw text 
loan=select(loan,-desc)

#url contains loan id which we already have in id column so no new info can be extracted from url
loan=select(loan,-url)

length(unique(loan$member_id))
length(unique(loan$id))

# as both id and member id are unique and and discrete so nothing can be extracted from that

loan=select(loan,-c(id,member_id))


# checking unique values in each column

sort(apply(loan, 2,function(x) length(unique(x))))


#removing column with only 1 unique value  



loan=select(loan,-c(pymnt_plan,initial_list_status,policy_code,application_type,acc_now_delinq,delinq_amnt ))

unique(loan$term)
unique(loan$tax_liens)
unique(loan$collections_12_mths_ex_med)
unique(loan$chargeoff_within_12_mths)

loan=select(loan,-c(tax_liens,collections_12_mths_ex_med,chargeoff_within_12_mths))

colSums(is.na(loan))

summary(loan$pub_rec_bankruptcies)

table(loan$pub_rec_bankruptcies)
table(loan$loan_status)


# removing emp_title  as there is a lot of variation
loan$emp_title=tolower(loan$emp_title)

emp_title=loan%>%
  group_by(emp_title)%>%
  summarise(n())
emp_title
# removing title as we already have purpose

loan=select(loan,-c(emp_title,title))

##removing columns which dont have much variance as they are not good for analysis
loan=loan[,-nearZeroVar(loan)]

#removing columns related to payments 
loan <- loan[,-c(27:33)]

################################# cleaning columns #####################


str(loan)

loan$term=str_extract(loan$term,'[\\d]+')
#loan=select(loan,-term)


loan$int_rate=str_extract(loan$int_rate,'[\\d,\\.]+')
#loan=select(loan,-int_rate)

table(loan$emp_length)
loan$emp_length_year=str_extract(loan$emp_length,"[\\d]+")
loan=select(loan,-emp_length)


#library(tidyr)
loan=separate(loan,"issue_d",into = c("issue_month","issue_year"),sep = "-")
table(loan$issue_year)
loan$issue_year=paste0("20",loan$issue_year)

loan=separate(loan,"earliest_cr_line",into = c("earliest_cr_line_month","earliest_cr_line_year"),sep = "-")
table(loan$earliest_cr_line_year)

loan=loan%>%
  mutate(earliest_cr_line_year=ifelse(earliest_cr_line_year<=99 & earliest_cr_line_year >20,paste0("19",earliest_cr_line_year),paste0("20",earliest_cr_line_year)))


loan$revol_util <- str_extract(loan$revol_util,'[\\d,\\.]+')
#loan=select(loan,-revol_util)

 

loan=separate(loan,"last_credit_pull_d",into = c("last_credit_pull_month","last_credit_pull_year"),sep = "-")
table(loan$last_credit_pull_year)
loan$last_credit_pull_year=ifelse(!is.na(loan$last_credit_pull_year),paste0("20",loan$last_credit_pull_year),loan$last_credit_pull_year)

#check data is sufficient
#percentage of defaulted loans in data set
nof_default_cases <-length(which(loan$loan_status=="Charged Off"))
default_cases_perc <-nof_default_cases/nrow(loan)
ifelse(default_cases_perc < 0.05, "data insufficient", "no issues")

####################
# na imputation
####################

colSums(is.na(loan))




loan$last_credit_pull_month=ifelse(is.na(loan$last_credit_pull_month),names(which.max(table(loan$last_credit_pull_month))),loan$last_credit_pull_month)
loan$last_credit_pull_year=ifelse(is.na(loan$last_credit_pull_year),names(which.max(table(loan$last_credit_pull_year))),loan$last_credit_pull_year)

loan$revol_util=as.numeric(loan$revol_util)

loan$emp_length_year=as.numeric(loan$emp_length_year)

loan=kNN(loan)
loan=loan[,1:30]

#####################
#derived metrics    #
#####################
loan$loan_amnt_bin=ifelse(loan$loan_amnt<=10000,"less than 10k",
                          ifelse(loan$loan_amnt>10000 & loan$loan_amnt<=20000,"10 to 20k",
                                 ifelse(loan$loan_amnt>20000 & loan$loan_amnt <=30000,"20 to 30k","greater than 30k")))

loan$installment_inc_per =(loan$installment/(loan$annual_inc/12))*100
  
######################################################
#Univariate analysis
######################################################
# loan amount is positively skewed with median at 10000 and there are peaks at multiple of 5 as people
#  go for mostly rounded loan amount 5k 10k 15k 20k
ggplot(loan,aes(loan_amnt))+geom_histogram(fill="orange",color="black",bins = 50)+theme_bw()
summary(loan$loan_amnt)

#check loan amount bins
ggplot(loan,aes(loan_amnt_bin))+geom_bar(fill="orange",color="black")+theme_bw()
table(loan$loan_amnt_bin)

# loan funded amount is positively skewed with median at 10000 and there are peaks at multiple of 5 as people
#  go for mostly rounded loan amount 5k 10k 15k 20k
ggplot(loan,aes(funded_amnt)) +geom_histogram(fill="orange",color="black",bins = 50)+theme_bw()
summary(loan$funded_amnt)

#most of the installments have less than 500 
summary(loan$installment)
ggplot(loan,aes(installment))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()
 

#
summary(loan$installment_inc_per)
ggplot(loan,aes(installment_inc_per))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()
 

loan$annual_inc=as.numeric(loan$annual_inc)
summary(loan$annual_inc)

##     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     4000   40404   59000   68969   82300 6000000 
ggplot(loan,aes(annual_inc))+geom_histogram(fill="orange",color="black", bins=50)+theme_bw()
# 

#from the above graphs we can see it follows a normal distribution
#there are many loans which have dti very high
#there is sudded drop dti >25
summary(loan$dti)
ggplot(loan,aes(dti))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()
# dti @25, 26

#there are very few having 1 or more delinq in last 2 years
summary(loan$delinq_2yrs)
table(loan$delinq_2yrs)
ggplot(loan,aes(delinq_2yrs))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()


summary(loan$inq_last_6mths)
table(loan$inq_last_6mths)
ggplot(loan,aes(inq_last_6mths))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()
#there are significant no of more than 1 inquires 

#summary(loan$mths_since_last_delinq)
#table(loan$mths_since_last_delinq)

#remove this check if it is removed above

#loan=select(loan,-mths_since_last_delinq)
#there are significant no of open accounts for most of the customers
table(loan$open_acc)
ggplot(loan,aes(open_acc))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()
summary(loan$open_acc)

##there are very few case which has public derogatory records
table(loan$pub_rec)
ggplot(loan,aes(pub_rec))+geom_histogram(fill="orange",color="black",bins = 5)+theme_bw()
summary(loan$pub_rec)

#as the revolving balance goes up their frequency decreases
ggplot(loan,aes(revol_bal))+geom_histogram(fill="orange",color="black", bins=50)+theme_bw()
summary(loan$revol_bal)

#most of the cases have multiple accounts
table(loan$total_acc)
ggplot(loan,aes(total_acc))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()
summary(loan$total_acc)

#total payment = total_rec_prncp  + total_rec_int   +   recoveries

#ggplot(loan,aes(last_pymnt_amnt))+geom_histogram(fill="orange",color="black",bins = 30)+theme_bw()
#summary(loan$last_pymnt_amnt)
#no need to study this 

###############################
#
#bivariate analysis
##############################

loan_num_col <- sapply(loan, is.numeric)
loan_num <- loan[,loan_num_col]


#Finding higly correlated features 
highly.cor <- findCorrelation(cor(loan_num), cutoff=0.85)
#"funded_amnt" "loan_amnt"   "installment" columns are highly co-related
names(loan_num[,highly.cor])

#Compute the correlation matrix
cormat <- round(cor(loan_num, use="na.or.complete"), digits=2)
head(cormat)

#Create the correlation heatmap with ggplot2
#The package reshape is required to melt the correlation matrix :

#library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

#The function geom_tile()[ggplot2 package] is used to visualize the correlation
#matrix :
#library(ggplot2)
ggheatmap <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile()

#Add correlation coefficients on the heatmap
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


#####################
#bivariate segmented
#####################
names(loan)

#there are more defaulters when the loan amount is greater than 30
prop.table(table(loan$loan_status,loan$loan_amnt_bin),1) *100
ggplot(loan,aes(x=loan_amnt_bin, fill=loan_status))+geom_bar(position = "fill")+theme_bw()
#summary(loan$loan_amnt_bin)
#matters
#---

# 
summary(loan$installment_inc_per)
#ggplot(loan,aes(x=installment_inc_per, fill=loan_status))+geom_histogram(position = "fill")+theme_bw()
lintr <- loan%>%group_by(loan_status)%>%summarise(med=round(median(installment_inc_per),2))
ggplot(loan,aes(x=loan_status, y=installment_inc_per)) + geom_boxplot(fill="orange")+geom_text(data = lintr, aes(x = loan_status, y = med, label = med), size = 3, vjust = -1.5)
#matters
#---

loan$int_rate=as.numeric(loan$int_rate)

l1<- loan%>%
  group_by(loan_status)%>%
  summarise(med=round(median(int_rate),2))
l1
#ggplot(l1,aes(x=loan_status,y=med)+geom_bar(position = "fill")+theme_bw()
ggplot(loan,aes(x=loan_status, y=int_rate)) + geom_boxplot(fill="orange")+geom_text(data = l1, aes(x = loan_status, y = med, label = med), size = 3, vjust = -1.5)
#matters
#----
#there are more chances of defaulter when the the perons is living in rented house
prop.table(table(loan$loan_status,loan$home_ownership),1)*100
ggplot(loan,aes(loan_status,fill=home_ownership))+geom_bar(position = "fill")+theme_bw()

#----
l2 <-loan%>%
  group_by(loan_status)%>%
  summarise(med=round(median(annual_inc),2))
l2
ggplot(loan,aes(x=loan_status, y=annual_inc)) + geom_boxplot(fill="orange")+geom_text(data = l2, aes(x = loan_status, y = med, label = med), size = 3, vjust = -1.5)

ggplot(loan,aes(log(annual_inc),fill=loan_status))+geom_density(alpha=0.6)
#ggplot(loan,aes(log(annual_inc),int_rate,color=factor(loan_status)))+geom_point()+geom_smooth()

# matters 
#----
#no concrete conclsion from verification status
prop.table(table(loan$loan_status,loan$verification_status),1)*100
ggplot(loan,aes(x=verification_status, fill=loan_status)) + geom_bar(position = "fill")
#----
#it looks like there is more probability of defaulters when issue month is oct/sep
#but it is not conclusive
prop.table(table(loan$loan_status,loan$issue_month),1)*100
ggplot(loan,aes(loan_status,fill=issue_month))+geom_bar(position = "fill")+theme_bw()

#----
# 
prop.table(table(loan$loan_status,loan$issue_year),1)*100
ggplot(loan,aes(x=issue_year, fill=loan_status)) + geom_bar(position = "fill")
#2011 issue_year
#--------

prop.table(table(loan$loan_status,loan$purpose),1)*100
# debt consolidation and small business has comparatively more defaulters
ggplot(loan,aes(x=purpose, fill=loan_status)) + geom_bar(position = "fill")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
#
#-----
length(unique(loan$addr_state))
prop.table(table(loan$loan_status,loan$addr_state),1)*100
#NE state has maximum ratio of defaulters
ggplot(loan,aes(x=addr_state, fill=loan_status)) + geom_bar(position="fill")
#-----
l3 <-loan%>%
  group_by(loan_status)%>%
  summarise(med=round(median(dti),2))
l3
ggplot(loan,aes(x=loan_status, y=dti)) + geom_boxplot(fill="orange")+geom_text(data = l3, aes(x = loan_status, y = med, label = med), size = 3, vjust = -1.5)
#matters

#------
loan%>%
  group_by(loan_status)%>%
  summarise(mean(delinq_2yrs))

prop.table(table(loan$loan_status,loan$delinq_2yrs),1)*100
ggplot(loan, aes(x=delinq_2yrs, fill=loan_status)) + geom_bar(position = "fill")
#------
prop.table(table(loan$loan_status,loan$earliest_cr_line_year),1)*100
ggplot(loan, aes(x=earliest_cr_line_year, fill=loan_status)) + geom_bar(position = "fill")
#2007
#-----
loan%>%
  group_by(loan_status)%>%
  summarise(median(inq_last_6mths))
prop.table(table(loan$loan_status,loan$inq_last_6mths),1)*100
#matters increasing proportionally
ggplot(loan, aes(x=inq_last_6mths, fill=loan_status)) + geom_bar(position = "fill")

#------
loan%>%
  group_by(loan_status)%>%
  summarise(median(open_acc))
prop.table(table(loan$loan_status,loan$open_acc),1)*100
ggplot(loan, aes(x=factor(open_acc), fill=loan_status)) + geom_bar(position = "fill")
#-----
names(loan)

loan%>%
  group_by(loan_status)%>%
  summarise(median(pub_rec))
prop.table(table(loan$loan_status,loan$pub_rec),1)*100
#matters
#frequency graph shows there are very few records with pub-rec 3 or 4
#however from second graph we can see there are significant no of defaulters when
#it is > 0
ggplot(loan, aes(x=factor(pub_rec), fill=loan_status)) + geom_bar()
ggplot(loan, aes(x=factor(pub_rec), fill=loan_status)) + geom_bar(position="fill")
#-----
loan$revol_util <- as.numeric(loan$revol_util)
loan%>%
  group_by(loan_status)%>%
  summarise(revol_util=round(median(revol_util, na.rm=T),2))
#if the revolviing utility is more than 75 percent then the chances of defaulter increases
ggplot(loan, aes(x=revol_util, fill=loan_status)) + geom_density(alpha=0.6)
table(is.na(loan$revol_util))
colSums(is.na(loan))
#matters





#-----

loan%>%
  group_by(loan_status)%>%
  summarise(median(total_acc))

#------
loan%>%
  group_by(loan_status)%>%
  summarise(median(int_rate_percent))
#need to check

#-----
loan$emp_length_year=as.numeric(loan$emp_length_year)
loan%>%
  group_by(loan_status)%>%
  summarise(median(emp_length_year,na.rm = T))

prop.table(table(loan$loan_status,loan$emp_length_year),1)*100

#--------
loan$revol_util_percent=as.numeric(loan$revol_util_percent)

loan%>%
  group_by(loan_status)%>%
  summarise(median(revol_util_percent,na.rm = T))

#We could suggest below recommendation based on analysis of the other variables so that   future loans does not become defaulters
#Annual Income- The loan should be avoided to be given to the one's having lower annual income.

#Loan Amount- The higher loan amounts are risk hence it should be properly verified while giving large loans.

#Purpose- The purpose of loan having small business should be avoided as they have chances of getting defaulted. Higher interest rates can be applied for small business loans.

#Address State-The loans applicant belonging to Nebarska should not be given loans as they are frequent defaulters. Higher interest rates should be applied and other factors should be properly checked before granting loan

#House Ownership-: The persons living in rented house have higher risk as there is higher percentage of defaulters . And loan should be granted after due diligence with respect to other factors

#Revolving Utility-: The higher revolving utility balance should be checked before giving loans as they are frequent defaulters.

#Inquiry during last 6 month- The customers which are making frequent inquires should be avoided giving loans as they are frequent defaulters.



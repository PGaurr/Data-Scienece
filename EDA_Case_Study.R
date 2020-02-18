# Gramener EDA Case Study 
# Input Dataset : loan.csv
# Output Dataset: loan_cleaned.csv
# Name : Swami Prem Pranav Kayashyap (APFE1786831)
#        Swarup Roy
#        Tuhin  Pal
#        Piyush Gaur
# ------------------------------------------------------
# Date : 1st Apr, 2018

#Import library
library(Hmisc)     # To imputes missing value
library(lubridate) # To manupulate date and time 
library(ggplot2)   # For plotting
library(gridExtra) # For plotting
library(stringr)   # String manupulation
library(scales)    # For plotting
library(reshape2)  # Melt data
library(corrplot)  # For Heatmap
library(dplyr)     # For mutate
#Import loan data from loan.csv 
loan <- read.csv(file.choose(),stringsAsFactors = F, na.strings = c("NA", "N/A", "", " "))

#####      1.  Data Cleaning starts     #####

#Removing column's having all NA
loan <- loan[,colSums(is.na(loan)) < nrow(loan)]

#Removing columns having only value as 0 or NA.
loan <- loan[, !apply((is.na(loan) | loan == 0), 2, all)]

## Removing columns with more than 50% NA
loan <- loan[, -which(colMeans(is.na(loan)) > 0.5)]

#Removing columns and Rows which is not important for this analysis

#URL, desc, title and emp_title is not needed as we are not going to do text based analysis for finding loan defaulter .
loan <- subset(loan, select = -c(url, desc, title, emp_title))

#loan status having value "Current"  is not required for for Defaulter analysis
loan<-loan[-which(toupper(loan$loan_status) == "CURRENT"), ]

# Removing columns having all value same
loan <- loan[vapply(loan, function(x) length(unique(x)) > 1, logical(1L))]

#convering all the non numeric columns to upper case 
loan <- data.frame(lapply(loan, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))

#converting int_rate and revol_util to numeric by removing the %
loan$int_rate <- as.numeric(gsub("%"," ",loan$int_rate))
loan$revol_util <- as.numeric(gsub("%"," ",loan$revol_util))

#Replacing all the missing value with median of the data set for pub_rec_bancrupties and revol_util.
loan$pub_rec_bankruptcies <- impute(loan$pub_rec_bankruptcies, median)
loan$revol_util <- impute(loan$revol_util)

#Cleanig of date data 

# Converting loan issued date in DEC-11 format to default format 2011-12-01 
loan$issue_d <- as.character(loan$issue_d)
loan$issue_d <- paste(loan$issue_d,"-01",sep = "")
loan$issue_d <- parse_date_time(loan$issue_d,"myd") 

# Converting earliest reported credit line date in DEC-11 format to default format 2011-12-01 
loan$earliest_cr_line <- as.character(loan$earliest_cr_line)
loan$earliest_cr_line <- paste(loan$earliest_cr_line,"-01",sep = "")
loan$earliest_cr_line <- parse_date_time(loan$earliest_cr_line,"myd")

#####       Data Cleaning ends     #####

write.csv(loan, file = "loan_cleaned.csv")

#seperating the loan dat into 2 data frames Charged Off and FullyPaid .
charged_Off <- loan[loan$loan_status== "CHARGED OFF",]
fully_paid <- loan[loan$loan_status == "FULLY PAID",]

#####    UNI VARIATE ANALYSIS     #####

#segmented univariate analysis on categorical variables

#Univariate analysis based on loan terms 
ggplot(charged_Off, aes(charged_Off$term , fill = term)) +
  geom_bar(stat= "count") +
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-0.2, position = position_dodge(0.2)) +
  xlab("Loan terms") +
  ylab("Total Applicants") +
  ggtitle(" No of charged off applicants Vs Loan terms")

#Univariate analysis based on grade 
ggplot(charged_Off, aes(charged_Off$grade , fill = grade)) +
  geom_bar(stat= "count") +
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-0.2, position = position_dodge(0.9)) +
  xlab(" Loan grades") +
  ylab("Total Applicants") +
  ggtitle(" No of applicants Vs Grade")

#Univariate analysis based on sub-grade 
ggplot(charged_Off, aes(charged_Off$sub_grade , fill = sub_grade)) +
  geom_bar(stat= "count") +
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-0.2, position = position_dodge(0.9)) +
  xlab("Loan sub-grades") +
  ylab("Total Applicant") +
  ggtitle(" No of applicants Vs Sub-grade(Interest rates)")

#grid.arrange(p1,p2,ncol=2)

#Univariate analysis on Employment length
loan <- loan[loan$emp_length != "N/A",]
loan$emp_length <- gsub("< 1 YEAR","0 YEARS",loan$emp_length)
loan$emp_length <- gsub("10+ YEARS","10 YEARS",loan$emp_length)
ggplot(loan, aes(loan$emp_length , fill = emp_length)) +
  geom_bar(stat= "count") +
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-0.1, position = position_dodge(0.9)) +
  xlab("Employment lengths") +
  ylab("Total Applicants") +
  ggtitle(" No of applicants Vs Employment length")

#Univariate analysis on  Home Ownership
ggplot(charged_Off, aes(charged_Off$home_ownership , fill = home_ownership))+
  geom_bar(stat= "count") +
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-0.1,position = position_dodge(0.9)) +
  xlab("Home ownership") +
  ylab("Total Applicants") +
  ggtitle("No of charged off applicants Vs Home_ownership")

#Univariate analysis on Verification Status
ggplot(charged_Off, aes(charged_Off$verification_status , fill = verification_status)) +
  geom_bar(stat= "count") +
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-0.1,position = position_dodge(0.9)) +
  xlab("Verification_status") + 
  ylab("Total Applicants") +
  ggtitle("No of charged off applicants Vs verification_status")

#Univariate analysis on Loan purpose

ggplot(charged_Off, aes(x = charged_Off$purpose , fill = purpose)) +
  geom_bar(stat= "count") + scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-0.1,position = position_dodge(0.9))+
  xlab("Purposes") +
  ylab("Total Applicants") +
  ggtitle("No of applicants Vs purpose")


#Univariate analysis on open acc
ggplot(charged_Off, aes(x=charged_Off$open_acc , fill = open_acc)) +
  geom_histogram(stat= "count") +
  geom_text(stat = "count",aes(label=(..count..)), vjust=-0.1, position = position_dodge(0.9)) +
  xlab("Types of loan based on open_acc") +
  ylab("Total Applicants") + 
  ggtitle("No of applicants Vs open_acc")

#Univariate analysis on deling 2 years
p1 <- ggplot(charged_Off, aes(x=charged_Off$delinq_2yrs , fill = delinq_2yrs)) +
  geom_bar(stat= "count") +
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-0.1,position = position_dodge(0.9)) +
  xlab("delinq_2yrs") +
  ylab("Total Applicant") +
  ggtitle("No of applicants Vs delinq_2yrs")

p2 <- ggplot(fully_paid, aes(x=fully_paid$delinq_2yrs , fill = delinq_2yrs)) +
  geom_bar(stat= "count") +
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-0.1,position = position_dodge(0.9)) +
  xlab("delinq_2yrs") +
  ylab("Total Applicants") +
  ggtitle("No of applicants Vs delinq_2yrs")

grid.arrange(p1, p2, ncol=2)
#No significant difference between these 2 graphs so we can ignore .

#Anaylsis on inq last 6 months 
p1<-ggplot(charged_Off,aes(x=inq_last_6mths , fill = inq_last_6mths )) +
  geom_bar(stat = "count") +
  geom_text(stat = "count",aes(label=(..count..)),vjust=-0.1,position = position_dodge(0.9)) +
  xlab("inq_last_6mths") +
  ylab("Total Applicants") +
  ggtitle("No of applicants Vs inq_last_6mths")
  
p2<-ggplot(fully_paid,aes(x=inq_last_6mths , fill = inq_last_6mths )) +
  geom_bar(stat = "count") +
  geom_text(stat = "count",aes(label=(..count..)),vjust=-0.1,position = position_dodge(0.9)) +
  xlab("inq_last_6mths") +
  ylab("Total Applicants") +
  ggtitle("No of applicants Vs inq_last_6mths")

grid.arrange(p1,p2,ncol=2)
#No significant difference between these 2 graphs so we can ignore.

#####         Bivariate analysis           #####

#Based on interest rate and loan status

ggplot(loan, aes(loan_status,int_rate)) +
  geom_boxplot() +
  labs(title = "Interest rate vs Loan Status", x = "Loan_status", y = "int_rate")
# We can see that int_rate might be significant factor in deciding defaulters.

#Based on loan status and total_payment_inv
ggplot(loan, aes(loan_status, total_pymnt_inv)) +
  geom_boxplot() +
  labs(title = "Total_payment_inv vs Loan Status", x = "Loan_status", y = "total_pymnt_inv")
#we can see that total_pymnt_inv might be significant factor in deciding defaulters

#Based on loan status and total_rec_prncp
ggplot(loan, aes(loan_status, total_rec_prncp)) +
  geom_boxplot() +
  labs(title = "Total received principle vs Loan Status", x = "Loan_status", y = "total_rec_prncp")
#we can see that total_rec_prncp can be a significant factor in deciding the defaulters

#boxplot recoveries and loanstatus
ggplot(loan, aes(loan_status,loan$recoveries)) +
  geom_boxplot() +
  labs(title = "Recoveries vs Loan Status", x = "Loan_status", y = "Revoveries")
# As for non-defaulters there is 0 recovery so it imples that it might not be useful for our analysis 

#boxplot loan_status and loan_amnt
ggplot(loan, aes(loan_status, loan_amnt)) +
  geom_boxplot() +
  labs(title = "Loan Amount vs Loan Status", x = "Loan_status", y = "loan_amnt")
#From boxplot it is intutive that loan_amnt might not help  to decide defaulters

#boxplot loan_status and funded_amnt
ggplot(loan,aes(loan_status,funded_amnt)) +
  geom_boxplot() +
  labs(title = "Funded Amount vs Loan Status", x = "Loan_status", y = "funded_amnt")
#From boxplot it seems that funded_amnt is not  be significant to decide defaulters

#boxplot loan_status and funded_amnt_inv
ggplot(loan, aes(loan_status, funded_amnt_inv)) +
  geom_boxplot() +
  labs(title = "Funded_amount_inv vs Loan Status", x = "Loan_status", y = "funded_amnt_inv")
#From boxplot it seems that funded_amnt_inv is not  be significant to decide defaulters

#boxplot loan_status installment 
ggplot(loan, aes(loan_status, installment)) +
  geom_boxplot() +
  labs(title = "Installment vs Loan Status", x = "Loan_status", y = "installment")
#From boxplot it seems that installment is not significant factor to decide defaulters

#boxplot loan_status annual_inc
ggplot(loan, aes(loan_status, annual_inc)) +
  geom_boxplot() +
  scale_y_continuous(labels = comma) +
  labs(title = "Annual Income vs Loan Status", x = "Loan_status", y = "annual_inc")
# From boxplot it seems that annual_incis not significant factor to decide defaulters.

#boxplot loan_status  open accounts
ggplot(loan, aes(loan_status,open_acc)) +
  geom_boxplot() +
  labs(title = "Open Accounts vs Loan Status", x = "Loan_status", y = "open_acc")
# From boxplot it seems that open_acc not significant factor to decide defaulters.

#boxplot loan_status and pub_rec
ggplot(loan, aes(loan_status,pub_rec)) +
  geom_boxplot() +
  labs(title = "pub_rec vs Loan Status", x = "Loan_status", y = "pub_rec")
# From boxplot it seems that pub_rec not significant factor to decide defaulters.

#Bar chart loan_status and pub_rec
p1 <- ggplot(charged_Off,aes(as.factor(pub_rec))) +
  geom_bar(stat= "count") +
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-1,position = position_dodge(0.9)) +
  labs(title = "On Charged off Data")
p2 <- ggplot(fully_paid,aes(as.factor(pub_rec))) +
  geom_bar(stat= "count") +
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-1,position = position_dodge(0.9)) +
  labs(title = "On Fully paid data")
grid.arrange(p1,p2, ncol = 2)
#We can see that both plots are identical,we cant derive much from pub_rec

#boxplot loan_status and revol_bal
ggplot(loan,aes(loan_status, revol_bal)) +
  geom_boxplot() +
  labs(title = "Revolving balance vs Loan Status", x = "Loan_status", y = "revol_bal")
# From boxplot it seems that revol_bal not significant factor to decide defaulters.

#boxplot loan_status and revol_util 
ggplot(loan,aes(loan_status,revol_util)) +
  geom_boxplot() +
  labs(title = "Revolving Utilization vs Loan Status", x = "Loan_status", y = "revol_util")
# From boxplot it seems that revol_util is significant factor to decide defaulters.

#boxplot loan_status and total account
ggplot(loan, aes(loan_status, total_acc)) +
  geom_boxplot() +
  labs(title = "Total acounts vs Loan Status", x = "Loan_status", y = "total_acc")
# From boxplot it seems that total_acc is not significant factor to decide defaulters.

#boxplot loan_status and total interst received
ggplot(loan,aes(loan_status,total_rec_int)) +
  geom_boxplot() +
  labs(title = "Total recieved interest vs Loan Status", x = "Loan_status", y = "total_rec_int")
# From boxplot it seems that total_rec_int is not significant factor to decide defaulters.

#boxplot loan_status and total_rec_late fee 
ggplot(loan,aes(loan_status, total_rec_late_fee)) +
  geom_boxplot() +
  labs(title = "Total recieved Late fee vs Loan Status", x = "Loan_status", y = "total_rec_late_fee")
# From boxplot it seems that total_rec_late_fee is not significant factor to decide defaulters.

#as recovery isn't important it will be always zero  for fully paid.So,It is related term same behaviour compare dto recovery.
ggplot(loan,aes(loan_status,collection_recovery_fee)) +
  geom_boxplot() +
  labs(title = "Collection recovery fee vs Loan Status", x = "Loan_status", y = "collection_recovery_fee")
# From boxplot it seems that collection_recovery_fee is not significant factor to decide defaulters.

#As median is same is for both and its not make any intuition to use last payment amount.
ggplot(loan,aes(loan_status,last_pymnt_amnt)) +
  geom_boxplot() +
  labs(title = "Last payment amount vs Loan Status",x = "Loan_status", y = "last_pymnt_amnt")
# From boxplot it seems that last_pymnt_amnt is not significant factor to decide defaulters.

#####       Bivariate analysis for Categorical Variables        ######

#home ownership vs loan status
ggplot(loan, aes( x = loan_status, group = home_ownership)) +
  geom_bar(stat="count",aes(y=..prop.., fill = factor(..x..))) +
  geom_text(aes(label = scales::percent(..prop..), y =..prop..), stat = "count" , vjust=-0.5 ) +
  facet_grid(~home_ownership) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels=abbreviate) +
  labs(y="Percent", x = "loan_status", title="Loan status vs Home Ownership")

#Purpose Vs Loan status 
ggplot(loan,aes( x = loan_status, group = purpose)) +
  geom_bar(aes( y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),y=..prop..), stat = "count", vjust= -0.5) +
  facet_grid(~purpose) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = abbreviate) +
  labs(y = "Percent", fill = "loan_status", title="Loan status vs purpose")

#Loan status vs employment length
ggplot(loan[loan$emp_length != "N/A",], aes(x = loan_status, group = emp_length)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count") +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop..), stat = "count", vjust= -.5) +
  facet_grid(~emp_length) +
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels = abbreviate) +
  labs(y = "Percent", fill = "Loan Status", title = "Loan status vs Employment length")

#Loan status vs grade 
ggplot(loan[loan$emp_length != "N/A",], aes(x = loan_status,  group = grade)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) +
  facet_grid(~grade) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels=abbreviate) +
  labs(y = "Percent", fill ="Loan Status", title = "Loan status vs grade")

#Loan status Vs verification status 
ggplot(loan, aes(x = loan_status,  group = verification_status)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count") +
  geom_text(aes( label = scales::percent(..prop..), y = ..prop.. ), stat = "count", vjust = -0.5) +
  facet_grid(~verification_status) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = abbreviate) +
  labs(y = "Percent", fill = "Loan status", title = "Loan status Vs verification status")

# Loan status Vs Address State
ggplot(loan, aes(x = loan_status,  group = addr_state)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count") +
  geom_text(aes( label = scales::percent(..prop..), y = ..prop.. ), stat = "count", vjust = -0.5) +
  facet_grid(~addr_state) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = abbreviate) +
  labs(y = "Percent", fill = "Loan status", title = "Loan status Vs Address state")
#Clearly One state (NEVADA) is showing maximum charged off cases. 

#####                    multivariate analysis                  #####

#Loan status , loan amount and purpose
ggplot(loan,aes(x = loan_amnt, y = purpose, col = loan_status)) +
  geom_point(alpha=0.5, position = "jitter")


#####       Correlation Matrix         #####

#Removing id and member_id as it is not usefull for correlation matrix
charged_Off <- subset(charged_Off, select = -c(id, member_id))
loan <- subset(loan, select = -c(id, member_id))

#Converting some numeric variable to factors as they have repeated datas.
loan$pub_rec_bankruptcies <- as.factor(loan$pub_rec_bankruptcies)
loan$pub_rec <-as.factor(loan$pub_rec)
loan$inq_last_6mths <-as.factor(loan$inq_last_6mths)
loan$delinq_2yrs <- as.factor(loan$delinq_2yrs)

#Deriving a new column credit loss in loan and charged_Off dataframe
loan$loss <- loan$funded_amnt - loan$total_rec_prncp
charged_Off$loss <- charged_Off$funded_amnt - charged_Off$total_rec_prncp

#Creating a data frame of all the numerical variables
numeric_charged_Off <- charged_Off[sapply(charged_Off,is.numeric)]
numeric_loan <- loan[sapply(loan,is.numeric)]

#Creating correlation matrix
corr_matrix <- round(cor(numeric_charged_Off),2)

#Creating lower triangle correlation matrix
get_lower_tri_corr_matrix <-function(corr_matrix){
  corr_matrix[upper.tri(corr_matrix)] <- NA
  return(corr_matrix)
}

#Creating upper triangle correlation matrix
get_upper_tri_corr_matrix <- function(corr_matrix){
  corr_matrix[lower.tri(corr_matrix)]<- NA
  return(corr_matrix)
}

# Use correlation between variables as distance
reorder_corr_matrix <- function(corr_matrix){
  dd <- as.dist((1 - corr_matrix)/2)
  hc <- hclust(dd)
  corr_matrix <- corr_matrix[hc$order, hc$order]
}

# Reorder the correlation matrix
corr_matrix <- reorder_corr_matrix(corr_matrix)

#Upper triangle correlation matrix
upper_tri_corr_matrix <- get_upper_tri_corr_matrix(corr_matrix)

# Melt the correlation matrix
melted_corr_matrix <- melt(upper_tri_corr_matrix, na.rm = TRUE)

# Create a ggheatmap
ggheatmap <- ggplot(melted_corr_matrix, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "green", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Leading Loan Parameters Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  coord_fixed()

ggheatmap + geom_text(aes(Var2, Var1, label = value), color = "black", size = 2) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal") +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top", title.hjust = 0.5))

#####      Result of Analysis done so far        ######

#Based on the analysis done through boxplot and bar chart, interest rate, total payment recieved,
#total payment investement, total recieved principle and revolve utilization are significant in 
#deciding the defaulters and these are highly correlated as we can see from above heatmap
#correlation's red region. So, We will eliminate other variables and focus more on these,
#we are subsetting the numeric variables for further analysis

sub_numeric_loan <- subset(numeric_loan, select = c(funded_amnt, int_rate, total_pymnt, total_pymnt_inv, total_rec_prncp, revol_util))

#Interest rate and total_rec_prncp Vs loan status
ggplot(loan, aes(x = int_rate, y = total_rec_prncp, color = loan_status)) +
  geom_point() +
  labs(title = "Effect of interest rate and total_rec_prncp on loan status")

#We can figure out that for less total_rec_prncp defaulters are high.
#we can also figure out that for moderate total_rec_prncp and moderate interest rate, defaulters are moderate.
#we can also figure out that for high total_rec_prncp, defaulters are very less.

#Interest rate and Loan Amount Vs loan status and loan term
ggplot(loan, aes(x = int_rate, y = loan_amnt, color = loan_status)) +
  geom_point() +
  facet_wrap(~term) +
  labs(title = "Effect of interest rate and Loan Amount on loan status")


#Interest rate and Loan Amount Vs loan status and loan pupose
ggplot(loan, aes(x= int_rate, y = loan_amnt, color = loan_status)) +
  geom_point() +
  facet_wrap(~purpose) +
  labs(title = "Effect of interest rate and Loan Amount on loan status")

#Divding the loan Amount into different levels 
loan <- mutate(loan,loan_amnt_labels=if_else(loan_amnt<=10000,'low_amount',
                                     if_else(total_rec_prncp>10000 & total_rec_prncp<=20000,'moderate_amount',
                                     if_else(total_rec_prncp>20000 & total_rec_prncp<=30000,'high_amount',
                                     'extreme_high_amount'))))

#Dividing the interest rate into different levels
loan <- mutate(loan, labels_int_rate = if_else(int_rate<=10,'low_interest',
                                       if_else(int_rate>10 & int_rate<=15,'mid_interest',
                                       if_else(total_rec_prncp>15 & total_rec_prncp<=20,'high_interest',
                                       'extreme_high_interest'))))

#Summarising the Values
grp_by_amt_int_rate <- summarise(
  group_by(loan,loan_amnt_labels,labels_int_rate),count=n())

grp_by_amt_int_rate_loan_status <- summarise(
  group_by(loan, loan_amnt_labels, labels_int_rate,loan_status), count = n())

merged_df_amt_int_rate <- merge(grp_by_amt_int_rate_loan_status, grp_by_amt_int_rate, by = c('loan_amnt_labels','labels_int_rate'))
merged_df_amt_int_rate$default_per <- merged_df_amt_int_rate$count.x/merged_df_amt_int_rate$count.y*100

#calulated percentage of defaulters
merged_df_amt_int_rate_charged_off <- subset(merged_df_amt_int_rate,loan_status=='CHARGED OFF')
ggplot(merged_df_amt_int_rate_charged_off, aes(loan_amnt_labels, default_per)) +
  geom_bar(stat="identity") +
  facet_wrap(~labels_int_rate) +
  labs(title ="Analysis based on interest rate vs Loan Amount")
#we can see clearly that
#Extremly high loan amount with Low interest can lead to 45% default
#Extremely high amount and high or mid level interest are contributing to 80% of defaults

#Group by Loan Term
grp_by_amount_interest_term <- summarise(group_by(loan, loan_amnt_labels, labels_int_rate, term), count=n())
grp_by_amount_interest_term_status <- summarise(group_by(loan, loan_amnt_labels, labels_int_rate, term, loan_status), count=n())

merge_df_amount_interest_term <- merge(grp_by_amount_interest_term, grp_by_amount_interest_term_status, by = c('loan_amnt_labels','labels_int_rate', 'term'))
merge_df_amount_interest_term$default_per <- merge_df_amount_interest_term$count.x/merge_df_amount_interest_term$count.y*100

#calulated percentage of defaulters
merge_df_amount_interest_term_charged_off <- subset(merge_df_amount_interest_term, loan_status == 'CHARGED OFF')
ggplot(merge_df_amount_interest_term_charged_off, aes(loan_amnt_labels, default_per)) +
  geom_bar(stat="identity") +
  facet_wrap(~labels_int_rate) +
  labs(title = "Analysis based on interest rate vs Loan Amount")
#This plot shows about the relatonship between, loan amount, interst rate, terms and loan status.

#Group by Loan Purpose
grp_by_amount_interest_purpose <- summarise(group_by(loan, loan_amnt_labels, labels_int_rate, purpose), count=n())
grp_by_amount_interest_purpose_status <- summarise(group_by(loan, loan_amnt_labels, labels_int_rate, purpose, loan_status), count=n())

merge_df_amount_interest_purpose <- merge(grp_by_amount_interest_purpose, grp_by_amount_interest_purpose_status, by = c('loan_amnt_labels','labels_int_rate', 'term'))
merge_df_amount_interest_purpose$default_per <- merge_df_amount_interest_purpose$count.x/merge_df_amount_interest_purpose$count.y*100

#calulated percentage of defaulters
merge_df_amount_interest_purpose_charged_off <- subset(merge_df_amount_interest_term, loan_status == 'CHARGED OFF')
ggplot(merge_df_amount_interest_purpose_charged_off, aes(loan_amnt_labels, default_per)) +
  geom_bar(stat="identity") +
  facet_wrap(~labels_int_rate) +
  labs(title = "Analysis based on interest rate vs Loan Amount")
#This plot shows about the relatonship between, loan amount, interst rate, purpose and loan status.

#Group by Home ownership
grp_by_amount_interest_home_ownership <- summarise(group_by(loan, loan_amnt_labels, labels_int_rate, home_ownership), count=n())
grp_by_amount_interest_home_ownership_status <- summarise(group_by(loan, loan_amnt_labels, labels_int_rate, home_ownership, loan_status), count=n())

merge_df_amount_interest_home_ownership <- merge(grp_by_amount_interest_home_ownership, grp_by_amount_interest_home_ownership_status, by = c('loan_amnt_labels','labels_int_rate', 'home_ownership'))
merge_df_amount_interest_home_ownership$default_per <- merge_df_amount_interest_home_ownership$count.x/merge_df_amount_interest_home_ownership$count.y*100

#calulated percentage of defaulters
merge_df_amount_interest_home_ownership_charged_off <- subset(merge_df_amount_interest_term, loan_status == 'CHARGED OFF')
ggplot(merge_df_amount_interest_home_ownership_charged_off, aes(loan_amnt_labels, default_per)) +
  geom_bar(stat="identity") +
  facet_wrap(~labels_int_rate) +
  labs(title = "Analysis based on interest rate vs Loan Amount")
#This plot shows about the relatonship between, loan amount, interst rate, home_ownership and loan status.

#####                                          ------END-----                                   #####
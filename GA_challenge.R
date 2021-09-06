#load libraries 

library(tidyverse)
library(ggplot2)
library(magrittr)
library(openxlsx)
library(mice)

# load csv files from remote

counts<-read_csv("https://raw.githubusercontent.com/sconnin/GA-Scenario/main/DataAnalyst_Ecom_data_sessionCounts.csv")

cart <- read_csv("https://raw.githubusercontent.com/sconnin/GA-Scenario/main/DataAnalyst_Ecom_data_addsToCart.csv")

#1.  review data and number of distinct values by col

glimpse(counts)
glimpse(cart)

counts %>% summarise_all(funs(n_distinct(.)))
cart %>% summarise_all(funs(n_distinct(.)))

# assess missing data

md.pattern(counts, plot=TRUE, rotate.names = TRUE)
md.pattern(cart, plot=TRUE, rotate.names = TRUE)

# 2. Clean and wrangle counts csv

sheet1<-counts%>%
    
# create Year and Month columns for grouping

separate(dim_date,  c("Month", "Day", "Year"))%>%

select(!Day)%>%
    
mutate(Month=recode(Month, '1' = 'January', '2' = 'February', '3' = 'March', 
                    
                    '4' = 'April', '5' = 'May', '6' = 'June', '7' = 'July', 
                    '8' = 'August','9' = 'September', '10' = 'October', 
                    '11' = 'November','12' = 'December'))%>%

mutate(Year=recode(Year, '12' = '2012', '13' = '2013'))%>%

# rename columns per instructions

rename(Transactions = transactions, Sessions = sessions, 

       Device = dim_deviceCategory, Browser = dim_browser)%>%

# create ECR ratio column per instructions

mutate(ECR = round(Transactions/Sessions,2))%>%

# replace 0 with NA for ECR where Sessions = 0 to avoid inf in summary stats

mutate(ECR=ifelse(Sessions == 0, NA, ECR))%>%

# remove non-value GA returns for Browser

filter(Browser != "(not set)" & Browser != "error" )

# Set factor and levels for Months column for custom arrange

sheet1$Month<-as.factor(cnts$Month)

sheet1$Month<- ordered(cnts$Month, levels = c('January', 'February', 'March',
        'April', 'May', 'June', 'July', 'August', 'September', 'October', 
        'November', 'December'))

# order columns starting with Year and Month to enhance review of chronology

sheet1%>%

relocate(Year, .before = Browser)%>%

relocate(Month, .after = Year)

# 3. Clean and wrangle carts csv for analysis

crt<-cart%>%

rename(Year = dim_year, Month = dim_month, AddsToCart = addsToCart)%>%

mutate(Month=recode(Month, '1' = 'January', '2' = 'February', '3' = 'March', 
        '4' = 'April', '5' = 'May', '6' = 'June', '7' = 'July', '8' = 'August', 
        '9' = 'September', '10' = 'October', '11' = 'November',
        '12' = 'December'))%>%

group_by(Year)%>%

#select last two months

tail(2)

# 4. Combine data sets to create sheet2 

#subset sheet1 and summarize data as instructed

AddsToCart<-crt$AddsToCart

sheet1b<-sheet1%>%

filter(Year==2013)%>%

filter(Month == 'May' | Month == 'June' )%>%

group_by(Year, Month)%>%

summarise_if(is.numeric, sum, na.rm = TRUE)

#combine datasets to create sheet2

glimpse(sheet2)

sheet2<-cbind(crt$AddsToCart, sheet1b)%>%

rename(AddsToCart = ...1)%>%

relocate(AddsToCart, .after = ECR)%>%

pivot_wider(names_from = Month, values_from = c(Sessions, Transactions, QTY, ECR, AddsToCart))%>%

mutate(Sess_Adiff = Sessions_June-Sessions_May, 
       Sess_RDiff = (Sessions_June-Sessions_May)/Sessions_June)%>%

relocate(c(Sess_Adiff, Sess_RDiff), .after = Sessions_June )%>%

mutate(Trans_Adiff = Transactions_June-Transactions_May, Trans_RDiff = (Transactions_June-Transactions_May)/Transactions_June)%>%

relocate(c(Trans_Adiff, Trans_RDiff), .after = Transactions_June )%>%

mutate(QTY_Adiff = QTY_June-QTY_May, QTY_RDiff = (QTY_June-QTY_May)/QTY_June)%>%

relocate(c(QTY_Adiff, QTY_RDiff), .after = QTY_June )%>%

mutate(Cart_Adiff = AddsToCart_June-AddsToCart_May, 
       Cart_RDiff = (AddsToCart_June-AddsToCart_May)/AddsToCart_June)%>%

relocate(c(Cart_Adiff, Cart_RDiff), .after = AddsToCart_June)%>%

rename(Sess_May=Sessions_May, Sess_June=Sessions_June, 
       Trans_May = Transactions_May, Trans_June = Transactions_June, 
       Cart_May = AddsToCart_May, Cart_June = AddsToCart_June)

# 5. write sheets to single xlxs file in home directory

excel <- list("Counts" = sheet1, "Two_Month_Compare" = sheet2)

#write.xlsx(excel, file = "challenge.xlsx")

# 6. Conduct Exploratory Data Analysis

(cnt_device<-sheet1%>%

    group_by(Year, Device)%>%

    #filter(Sessions != 0) %>% #note took out zer sessions

    summarise(mn_sess = mean(Sessions, na.rm=T), 
        sd_sess = sd(Sessions, na.rm=T), 
        mn_trans=mean(Transactions, na.rm=T), 
        sd_trans = sd(Transactions, na.rm=T),
        mn_ecr = round(mean(ECR, na.rm=T),3), 
        sd_ecr=round(sd(ECR, na.rm=T), 3)))
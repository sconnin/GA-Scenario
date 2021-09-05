#load libraries 

library(tidyverse)
library(ggplot2)
library(magrittr)
library(openxlsx)

# load csv files from remote

counts<-read_csv("https://raw.githubusercontent.com/sconnin/GA-Scenario/main/DataAnalyst_Ecom_data_sessionCounts.csv")
cart <- read_csv("https://raw.githubusercontent.com/sconnin/GA-Scenario/main/DataAnalyst_Ecom_data_addsToCart.csv")

# review data

glimpse(counts)
glimpse(cart)

unique(counts$dim_browser)

counts %>% summarise_all(funs(n_distinct(.)))
cart %>% summarise_all(funs(n_distinct(.)))


# 1. Clean and wrangle counts csv

cnts<-counts%>%
    
    separate(dim_date,  c("Month", "Day", "Year"))%>%
    
    mutate(Month=recode(Month, '1' = 'January', '2' = 'February', '3' = 'March', 
        '4' = 'April', '5' = 'May', '6' = 'June', '7' = 'July', '8' = 'August', 
        '9' = 'September', '10' = 'October', '11' = 'November', 
        '12' = 'December'))%>%
    
    mutate(Year=recode(Year, '12' = '2012', '13' = '2013'))%>%
    
    # rename columns per instructions
    
    rename(Transactions = transactions, Sessions = sessions, 
        Device = dim_deviceCategory, Browser = dim_browser)%>%
    
    # create ECR ratio column per instructions

    mutate(ECR = Transactions/Sessions)
    
# Replace NaaN with 0 for ECR
    
cnts$ECR[is.na(cnts$ECR)] <- 0 

# Set factor and levels for Months column for custom arrange

cnts$Month<-as.factor(cnts$Month)

cnts$Month<- ordered(cnts$Month, levels = c('January', 'February', 'March',
        'April', 'May', 'June', 'July', 'August', 'September', 'October', 
        'November', 'December'))

# Aggregate and arrange data for easier viewing 

sheet1<-cnts%>%
    
    #subset columns per instructions
    
    select(Month, Browser, Device, Sessions, Transactions, QTY, ECR)%>%
    
    # Group data per instructions

    group_by(Month, Browser, Device)%>%
    arrange(Month, .by_group = TRUE)%>%
    
    # Remove rows with 'error' or 'not set' returns in Browser column
    
    filter(Browser != "(not set)" & Browser != "error" )%>%

    #Add Year col for data integrity

    add_column(Year = 2013)%>%
    relocate(Year, .before = Month)

# 2. Clean and wrangle carts csv for analysis

crt<-cart%>%

    rename(Year = dim_year, Month = dim_month, AddsToCart = addsToCart)%>%
    
    mutate(Month=recode(Month, '1' = 'January', '2' = 'February', '3' = 'March', 
        '4' = 'April', '5' = 'May', '6' = 'June', '7' = 'July', '8' = 'August', 
        '9' = 'September', '10' = 'October', '11' = 'November', 
        '12' = 'December'))%>%
    
    group_by(Year)%>%
    tail(2)

# 3. Combine data sets to create tbl2 

#subset crt and summarize cnts data by month as instructed

AddsToCart<-crt$AddsToCart

cnts2<-cnts%>%
    filter(Year==2013)%>%
    filter(Month == 'May' | Month == 'June' )%>%
    filter(Browser != "(not set)" & Browser != "error" )%>%
    group_by(Month)%>%
    summarise_if(is.numeric, sum, na.rm = TRUE)



#combine datasets to create tbl2

sheet2<-cbind(AddsToCart, cnts2)%>%
    relocate(AddsToCart, .after = ECR)%>%
    pivot_wider(names_from = Month, values_from = c(Sessions, Transactions, QTY, ECR, AddsToCart))%>%
    
    mutate(Sess_Adiff = Sessions_June-Sessions_May, Sess_RDiff = (Sessions_June-Sessions_May)/Sessions_June)%>%
    relocate(c(Sess_Adiff, Sess_RDiff), .after = Sessions_June )%>%
    
    mutate(Trans_Adiff = Transactions_June-Transactions_May, Trans_RDiff = (Transactions_June-Transactions_May)/Transactions_June)%>%
    relocate(c(Trans_Adiff, Trans_RDiff), .after = Transactions_June )%>%
    
    mutate(QTY_Adiff = QTY_June-QTY_May, QTY_RDiff = (QTY_June-QTY_May)/QTY_June)%>%
    relocate(c(QTY_Adiff, QTY_RDiff), .after = QTY_June )%>%
        
    mutate(Cart_Adiff = AddsToCart_June-AddsToCart_May, Cart_RDiff = (AddsToCart_June-AddsToCart_May)/AddsToCart_June)%>%
    relocate(c(Cart_Adiff, Cart_RDiff), .after = AddsToCart_June)%>%
    
    rename(Sess_May=Sessions_May, Sess_June=Sessions_June, Trans_May = Transactions_May, Trans_June = Transactions_June, Cart_May = AddsToCart_May, Cart_June = AddsToCart_June)%>%

    #Add Year col for data integrity
    
    add_column(Year = 2013)%>%
    relocate(Year, .before = Sess_May)

# 2. Clean and wrangle carts csv for analysis
    
# write tbls to single xlxs file in home directory

excel <- list("Counts" = sheet1, "Two_Month_Compare" = sheet2)
write.xlsx(excel, file = "challenge.xlsx")


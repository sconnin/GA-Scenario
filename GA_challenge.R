#load libraries 

library(tidyverse)
library(ggplot2)
library(magrittr)
library(openxlsx)
library(mice)
library(viridis)

# load csv files from remote

counts<-read_csv("https://raw.githubusercontent.com/sconnin/GA-Scenario/main/DataAnalyst_Ecom_data_sessionCounts.csv")
cart <- read_csv("https://raw.githubusercontent.com/sconnin/GA-Scenario/main/DataAnalyst_Ecom_data_addsToCart.csv")

# 1. Initial EDA

# Review data structure and components

str(counts)
str(cart)

# Get number of unique values for categorical features to support downstream analysis

counts %>%
    summarise(across(c(dim_browser, dim_deviceCategory, dim_date), 
                     n_distinct, rm.na=T))

cart%>%
    summarise(across(c(dim_year, dim_month), n_distinct, rm.na=T))

#Assess missing values to avoid summary stat errors

md.pattern(counts, plot=TRUE, rotate.names = TRUE)
md.pattern(cart, plot=TRUE, rotate.names = TRUE)


# 2. Clean and wrangle counts csv

sheet1<-counts%>%
    
    # create Month, Day, Year columns for grouping
    
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

    mutate(ECR = round(Transactions/Sessions, 2))%>%
    
    # replace 0 with NA for ECR where Sessions = 0 to avoid inf in summary stats
    
    mutate(ECR=ifelse(Sessions == 0, NA, ECR))%>%

    # remove non-value GA returns for Browser
    
    filter(Browser != "(not set)" & Browser != "error" )%>%

    # order columns starting with Year and Month to enhance review of chronology
    
    relocate(Year, .before = Browser)%>%
    relocate(Month, .after = Year)%>%
    relocate(Day, .after = Month)%>%

# Aggregate data per instructions

    group_by(Month, Device)
      

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

#subset crt and sheet1 as instructed

AddsToCart<-crt$AddsToCart

sheet1b<-sheet1%>%
    filter(Year==2013)%>%
    filter(Month == 'May' | Month == 'June' )%>%
    group_by(Year, Month)%>%
    summarise_if(is.numeric, sum, na.rm = TRUE)


#combine datasets to create sheet2

sheet2<-cbind(AddsToCart, sheet1b)%>%
    rename(AddsToCart = ...1)%>%
    relocate(AddsToCart, .after = ECR)%>%
    pivot_wider(names_from = Month, values_from = c(Sessions, 
    Transactions, QTY, ECR, AddsToCart))%>%
    
    mutate(Sess_Adiff = Sessions_June-Sessions_May, 
         Sess_RDiff = (Sessions_June-Sessions_May)/Sessions_June)%>%
    relocate(c(Sessions_June,Sess_Adiff, Sess_RDiff), .after = Sessions_May )%>%
    
    mutate(Trans_Adiff = Transactions_June-Transactions_May, 
        Trans_RDiff = (Transactions_June-Transactions_May)/Transactions_June)%>%
    relocate(c(Transactions_June,Trans_Adiff, Trans_RDiff), .after = Transactions_May)%>%
    
    mutate(QTY_Adiff = QTY_June-QTY_May, 
        QTY_RDiff = (QTY_June-QTY_May)/QTY_June)%>%
    relocate(c(QTY_June, QTY_Adiff, QTY_RDiff), .after = QTY_May )%>%
    
    mutate(ECR_Adiff = ECR_June-ECR_May, 
           ECR_RDiff = (ECR_June-ECR_May)/ECR_June)%>%
    relocate(c(ECR_June, ECR_Adiff, ECR_RDiff), .after = ECR_May )%>%
        
    mutate(Cart_Adiff = AddsToCart_June-AddsToCart_May, 
        Cart_RDiff = (AddsToCart_June-AddsToCart_May)/AddsToCart_June)%>%
    relocate(c(AddsToCart_June, Cart_Adiff, Cart_RDiff), .after = AddsToCart_May)%>%
    
    rename(Sess_May=Sessions_May, Sess_June=Sessions_June, 
        Trans_May = Transactions_May, Trans_June = Transactions_June, 
        Cart_May = AddsToCart_May, Cart_June = AddsToCart_June)

# 5. write sheets to single xlxs file in home directory

excel <- list("Counts" = sheet1, "Two_Month_Compare" = sheet2)
write.xlsx(excel, file = "GA_challenge.xlsx")

# 6. Stats and Graphs

# Set factor and levels for Months column for custom arrange

sheet1$Month<-as.factor(sheet1$Month)

sheet1$Month<- ordered(sheet1$Month, levels = c('January', 'February', 'March',
'April', 'May', 'June', 'July', 'August', 'September', 'October', 
'November', 'December'))

#create summary statistics

cnt_month<-sheet1%>%
    group_by(Month, Device, Browser)%>%
        summarise(
        tot_sess = sum(Sessions),
        mn_sess = mean(Sessions, na.rm=T), 
        sd_sess = sd(Sessions, na.rm=T),
        tot_trans = sum(Transactions),
        mn_trans=mean(Transactions, na.rm=T), 
        sd_trans = sd(Transactions, na.rm=T),
        mn_ecr = round(mean(ECR, na.rm=T),3), 
        sd_ecr=round(sd(ECR, na.rm=T), 3))

#Total Sessions by Month

s<-cnt_month%>%
    ggplot(aes(x=Month, y=tot_sess, fill=Device))+
    geom_col(alpha =  0.8)+
    scale_x_discrete(limits = rev(levels(levels)))+
    scale_fill_viridis_d(option="E")+
    ggtitle("Sessions Per Month") + 
    theme(plot.title = element_text(
        size = 10,
        lineheight = .9,
        family = "Times",
        colour = "black"))+
    ylab("Total Count")+
    theme(axis.text.y = element_text(
        size=8))+
    xlab("Month")+
    theme(axis.text.x = element_text(
        size=8))+
    theme_classic()

sess<-s+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))

#Total Transactions by Month

t<-cnt_month%>%
    ggplot(aes(x=Month, y=tot_trans, fill=Device))+
    geom_col(alpha =  0.8)+
    scale_x_discrete(limits = rev(levels(levels)))+
    scale_fill_viridis_d(option="E")+
    ggtitle("Transactions Per Month") + 
    theme(plot.title = element_text(
        size = 10,
        lineheight = .9,
        family = "Times",
        colour = "black"))+
    ylab("Total Count")+
    theme(axis.text.y = element_text(
        size=8))+
    xlab("Month")+
    theme(axis.text.x = element_text(
        size=8))+
    theme_classic()

trans<-t+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))

#Average eConversion per Month

c<-cnt_month%>%
    ggplot(aes(x=Month, y=mn_ecr, fill=Device))+
    geom_col(alpha =  0.8)+
    scale_x_discrete(limits = rev(levels(levels)))+
    scale_fill_viridis_d(option="E")+
    ggtitle("Conversion Rate Per Month") + 
    theme(plot.title = element_text(
        size = 10,
        lineheight = .9,
        family = "Times",
        colour = "black"))+
    ylab("Average")+
    theme(axis.text.y = element_text(
        size=8))+
    xlab("Month")+
    theme(axis.text.x = element_text(
        size=8))+
    theme_classic()

conv<-c+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))

# Role of Device and Browser on Transactions

dev_trans<-counts%>%
  select(dim_browser, dim_deviceCategory, transactions)%>%
  rename(Browser = dim_browser, Device = dim_deviceCategory,
  Transactions=transactions)%>%
  group_by(Device)%>%
  summarise(across(where(is.numeric), 
      list(mean = mean, sum = sum)))

dev_trans%>%
  mutate_at(vars(contains('Device')),as.factor)%>%
  ggplot(aes(x=reorder(Device, Transactions_sum), 
    y =Transactions_sum))+
  geom_col(aes(fill=Device), width=0.4, show.legend = FALSE)+
  coord_flip()+
  scale_fill_viridis_d(option="E")+
  ggtitle("Total Transactions by Device") + 
  theme(plot.title = element_text(
    size = 10,
    lineheight = .9,
    family = "Times",
    colour = "black",
    vjust = 5))+
  ylab("Total")+
  theme(axis.text.y = element_text(
    size=8))+
  xlab("Device")+
  theme(axis.text.x = element_text(
    size=8))+
  theme_classic()

#Role of Browser by device

(brows_desk<-counts%>%
  filter(dim_deviceCategory == 'desktop')%>%
  select(dim_browser,  transactions)%>%
  rename(Browser = dim_browser, Transactions=transactions)%>%
  group_by(Browser)%>%
  summarise(across(where(is.numeric), list(sum = sum)))%>%
  filter(Transactions_sum >1000)%>%
  arrange(desc(Transactions_sum))%>%
  mutate_at(vars(contains('Browser')),as.factor)%>%
  ggplot(aes(x=reorder(Browser, Transactions_sum), 
             y =Transactions_sum))+
  geom_col(aes(fill=Browser), width=0.4, show.legend = FALSE)+
  coord_flip()+
    scale_fill_viridis_d(option="E")+
    ggtitle("Transactions by Desktop Browser:\n Where Total > 1000") + 
    theme(plot.title = element_text(
      size = 10,
      lineheight = .9,
      family = "Times",
      colour = "black",
      vjust = 5))+
    ylab("Total")+
    theme(axis.text.y = element_text(
      size=8))+
    xlab("Browser")+
    theme(axis.text.x = element_text(
      size=8))+
  theme_classic())

# Transactions vs Sessions by Device

trans_sess<-sheet1%>%
  filter(!(ECR > 1))%>%
  ggplot(aes(x=Sessions, y=Transactions))+
  geom_point(aes(color=Device))+
  ggtitle("Transactions by Session Count and Device")+ 
  theme(plot.title = element_text(
    size = 10,
    lineheight = .9,
    family = "Times",
    colour = "black",
    vjust = 5))+
  ylab("Total Transactions")+
  theme(axis.text.y = element_text(
    size=6))+
  xlab("Total Sessions")+
  theme(axis.text.x = element_text(
    size=6))+
  theme_classic()








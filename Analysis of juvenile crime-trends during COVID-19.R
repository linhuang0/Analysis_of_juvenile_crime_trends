
library(data.table)
library(dplyr)
library(lmtest)
library(ggplot2)
library(kableExtra)
library(stringr)
library(rmarkdown)
library(vars)
library(forecast)
library(ggfortify)
library(gridExtra)
library(changepoint)
library(seasonal)


#crime Data 
ANZSOC_AEG_Data<-fread ("ANZSOC_AEG_Full Data_data.csv",stringsAsFactors=TRUE) 
colnames(ANZSOC_AEG_Data)
sum(ANZSOC_AEG_Data$Proceedings)

#sum by Type  and so on
crime_data<-aggregate(ANZSOC_AEG_Data$Proceedings,by=list(ANZSOC_AEG_Data$`ANZSOC Division`,ANZSOC_AEG_Data$`Year Month`,ANZSOC_AEG_Data$Ethnicity,ANZSOC_AEG_Data$`Age Group`,ANZSOC_AEG_Data$`Police District`),sum)
colnames(crime_data)<- c("Type","Year Month","Ethnicity","Age","District","Number")
levels(ANZSOC_AEG_Data$`ANZSOC Division`)
summary(crime_data)
crime_selected_type<-filter(crime_data,crime_data$Type =="Unlawful Entry With Intent/Burglary, Break and Enter"|crime_data$Type=="Sexual Assault and Related Offences" |crime_data$Type=="Robbery, Extortion and Related Offences" |crime_data$Type=="Theft and Related Offences" )
levels(crime_selected_type$`Year Month`)
crime_selected_type$`Year Month`<-gsub('[-]','',crime_selected_type$`Year Month`)
crime_selected_type$`Year Month`<-as.yearmon(crime_selected_type$`Year Month`, "%b%y")
levels(crime_selected_type$`Ethnicity`)
#crime_selected_type$Age<-gsub('Oct','12',crime_selected_type$Age)
crime_selected_type_age<-filter(crime_selected_type,crime_selected_type$Age =="15-19"|crime_selected_type$Age =="20-24")

crime_total_data<-aggregate(crime_selected_type_age$Number,by=list(crime_selected_type_age$`Year Month`),sum)
colnames(crime_total_data)<- c("Year Month","Number")

ggplot(crime_total_data, aes(`Year Month`, Number))+ geom_point()+geom_smooth(method="lm")+ggtitle("Crime number Monthly")
ggplot(data=crime_total_data,aes(x=`Year Month`,y=Number))+geom_line()+ggtitle("Crime number Monthly")

mean(crime_total_data$Number)
var(crime_total_data$Number)
median(crime_total_data$Number)

crime_type_data<-aggregate(crime_selected_type_age$Number,by=list(crime_selected_type_age$`Year Month`,crime_selected_type_age$Type,crime_selected_type_age$District),sum)
colnames(crime_type_data)<- c("Year Month","Type","Number","District")

ggplot(data=crime_type_data, aes(x=`Year Month`, y=Number)) +geom_point(alpha = 0.1) +facet_grid(~Type)
#ggplot(data=crime_type_data, aes(x=`Year Month`, fill = Type)) +geom_histogram(bins = 50)


crime_Ethnicity_data<-aggregate(crime_selected_type_age$Number,by=list(crime_selected_type_age$`Year Month`,crime_selected_type_age$Ethnicity),sum)
colnames(crime_Ethnicity_data)<-c("Year Month","Ethnicity","Number")

crime_Ethnicity_data$Ethnicity[crime_Ethnicity_data$Ethnicity =="Not Elsewhere Classified"|crime_Ethnicity_data$Ethnicity =="Organisation"|crime_Ethnicity_data$Ethnicity =="Not Stated"]<-NA
crime_Ethnicity_data<-na.omit(crime_Ethnicity_data)

#barplot
ggplot(data=crime_Ethnicity_data, aes(x=Ethnicity, y=Number,fill=`Year Month`)) +geom_bar(stat="identity",color="black")

# line types
ggplot(data=crime_Ethnicity_data, aes(x=`Year Month`, y=Number, group=Ethnicity,color=Ethnicity)) +
  geom_line(linetype="dashed", size=0.6)+geom_smooth(method="loess")  
geom_point(color="red", size=1)

ggplot(data=crime_Ethnicity_data, aes(x=Number, y=Ethnicity,fill =Ethnicity)) +geom_boxplot()+coord_flip()

crime_district_data<-aggregate(crime_selected_type_age$Number,by=list(crime_selected_type_age$District),sum)
summary(crime_district_data)
colnames(crime_district_data)<-c("District","Number")

#barplot
ggplot(data=crime_district_data, aes(x=District, y=Number,fill=District)) +geom_bar(stat="identity",color="black")


crime_before_data<-crime_total_data[crime_total_data$`Year Month`<'Feb 2020',]
crime_after_data<-crime_total_data[crime_total_data$`Year Month`>='Feb 2020',]
crime_before_after_compare<-crime_before_data
crime_before_after_compare$YearMonth<-c(1:30)
crime_before_after_compare$beforeNum<-as.numeric(crime_before_data$Number)
crime_before_after_compare$afterNum<-as.numeric(crime_after_data$Number)
crime_before_after_compare$`Year Month`<-NULL
crime_before_after_compare$Number<-NULL
ggplot(crime_before_after_compare, aes(x=YearMonth)) + 
  geom_point(aes(y=beforeNum) , ) + geom_line(aes(y=beforeNum,color="cyan")) +
  geom_point(aes(y=afterNum), ) + geom_line(aes(y=afterNum,color="red"))+ ggtitle("Crime number Monthly compare")


Crime_Offenders_timeseries<-ts(crime_total_data,start(2017,8),frequency=12)
autoplot(Crime_Offenders_timeseries,facets = TRUE) + ggtitle("The Time Series of crime")
a <- autoplot(ma(Crime_Offenders_timeseries,3)) 
b <- autoplot(ma(Crime_Offenders_timeseries,7)) 
c <- autoplot(ma(Crime_Offenders_timeseries,10))
d <- autoplot(Crime_Offenders_timeseries)
grid.arrange(d,a,b,c,ncol=2)
##forecast
forecast_Crime_Offenders_timeseries<-forecast(Crime_Offenders_timeseries)
autoplot(forecast_Crime_Offenders_timeseries)+ ggtitle("Forecast")
##change points
shapiro.test(crime_total_data$Number) 
#cpm.res = processStream(crime_total_data$Number, cpmType = "Kolmogorov-Smirnov")
plot(crime_total_data, type = "l", col = "steelblue", lwd = 2) 
#+abline(v=cpm.res$changePoints)
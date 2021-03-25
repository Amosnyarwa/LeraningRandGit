
getwd()
## standard practice is to load libraries At the top of your script
 library(tidyverse)
# the working directory is now set to where the  .Rproj file is located 
getwd()
#read in csv using readr package
df<- read_csv(file = "inputs/reach_ssd_dataset_joint_market_monitoring_initiative_jmmi_december_2019_trader_data.csv")
df
# inspect the data set
class(df)
dim(df)
nrow(df)
ncol(df)
colnames(df)
df$chicken_price<-df$chicken_price
vec_chicken_price<-df$chicken_price


df
df %>%
  group_by(state) %>%  
  summarise(
    mean_bean_price=mean(beans_price,na.rm=T))

prices_per_county<-df %>%
  group_by(state) %>%  
  summarise(
    mean_charcoal_price=mean(charcoal_price,na.rm=T),
    mean_rice_rice=mean(rice_price,na.rm=T),
    max_rice_price=max(rice_price,na.rm=T),
    min_rice_price=min(rice_price,na.rm=T))
prices_per_county
write_csv(x = prices_per_county,file="outputs/ssd_jimmi_prices_per_county,file="outputs/ssd_jimmi_prices_per_county.csv")


prices_per_county$mean_charcoal_price %>% hist()
hist(prices_per_county$mean_charcoal_price)
prices_per_county %>% 
filter(mean_charcoal_price>=300)
 prices_per_county %>% 
ggplot(aes(x=county, y=mean_charcoal_price,fill=county)+geom_bar(stat="identity"))

prices_per_county$mean_rice_price %>% hist()
hist(prices_per_county$mean_rice_price)
prices_per_county %>% 
filter(mean_rice_price>=3500)
 prices_per_county %>% 
ggplot(aes(x=county, y=mean_rice_price,fill=county)+geom_bar(stat="identity)

write_csv(x=prices_per_county)

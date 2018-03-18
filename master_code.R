source("support_scripts.R")      # a script i call that has functions to scrape and clean the cansim data

library(tidyverse)
library(lubridate)
library(gganimate)
library(scales)

### Get and wrangle the data ###
data <- getCANSIM2("2820135")
data <- convertdate(data)
data <- renameprov(data)
data <- renameCMA(data)
data <- cleanCANSIM(data)
data$nchar<-nchar(data$GEO)
data <- data %>%
  filter(CHARACTERISTICS=="Unemployment rate (percentage)",
         STATISTICS=="Estimate",
         DATATYPE=="Seasonally adjusted",
         nchar>3) %>%
  select(GEO,Value:date) #keep only certain columns
data$prov=substr(data$GEO,nchar(data$GEO)-1,nchar(data$GEO)) #strip out the province from the CMA strings
data$GEO <- factor(data$GEO,levels=CMAlist)
data$prov <- factor(data$prov,levels=c("BC","AB","SK","MB","ON","QC","NB","NS","NL"))
data$fct_date<-as.factor(data$date)

### In the end i've got a dataframe (data) with Unemployment Rates (y=Value) and CMA categorical variables (x=GEO)
### I also have a useful grouping variable of province (prov)


### Now, ggplot ###
### It's just a geom_bar plot with the trick being to add coord_polar. That's it.

lastframe<-data %>%
  filter(fct_date=="2018-02-01") #set to latest monthly data, could probably use a max() function here...

lastframe[1:1020,]<-lastframe[1:34,] #repeat latest data 30 times to create ending pause effect

for(r in 35:1020){
  lastframe$date[r]<-as.Date(lastframe$date[r-34] %m+% months(1)) #dummy the date variable so it will keep going in gganimate for the lastdata paused data
}
lastframe$fct_date<-as.factor(lastframe$date)
pausedata<-rbind(data,lastframe)  #includes the repeated lastdate data with dummied dates

g<-ggplot(pausedata,aes(x=GEO,y=Value,frame=fct_date,fill=Value))+
  geom_bar(stat="identity",position="identity",alpha=.7)+
  geom_text(aes(label=Value),color="grey20",hjust=0.5,size=3)+
  geom_label(aes(x=0,y=0,label=paste0(year,"-",month)),size=4)+
  scale_fill_gradient2("UR",low = muted("green"), mid = "yellow",
                       high = "red", midpoint = 5)+
  scale_y_continuous(limits=c(0,12),oob=squish)+   #oob=squish makes all the "out of bound" values display a bar up to the max value set by the limit (otherwise the bar vanishes); uses library(scales)
  coord_polar()+
  labs(title="Unemployment rate by CMA",
       y="",
       x="",
       caption="Source: CANSIM 282-0135\nChart by @bcshaffer")+
  theme(text=element_text(family="Avenir"),
        panel.grid.major.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_text(size=8),
        axis.ticks.y=element_blank(),
        plot.caption = element_text(size=rel(0.7),face="italic"),
        plot.background = element_rect(
          fill = "grey92",
          colour = "black",
          size = 1
        ))
gganimate(g,"unemployment_polar_gg.gif",title_frame = F, ani.height=600,ani.width=450,interval=.2)

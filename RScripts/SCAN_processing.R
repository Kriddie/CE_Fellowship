#SCAN!
#Kriddie Whitmore
  #2022-01-21

##partial least sq
#plsr (r package)
#spectral slope
#hec ras - stream geometery to identify flow

library(here)
library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)

SCAN_df <- read.delim(here::here("/FallData/SCAN/211029-1/2021-06-22_15-09-00.txt"),
                      skip = 1,
                      stringsAsFactors=FALSE)

names(SCAN_df)[names(SCAN_df) == 'Date.Time'] <- 'DateTime'

#format DateTime 
SCAN_df$DateTime <- gsub('[.]', '-', SCAN_df$DateTime)
SCAN_df$DateTime <- as.POSIXct(SCAN_df$DateTime, 
                               format = "%Y-%m-%d %H:%M:%S",tz="UTC")

SCAN_df$DateTime <- round_date(SCAN_df$DateTime, unit = "1 minute")

#Clip to period of study
  #START: 2021-10-20 18:24:00
  #END: 2021-10-22 14:28

SCAN_Fall <- 
  subset(SCAN_df,
         DateTime > as.POSIXct('2021-10-20 18:24:00', tz="UTC") & 
         DateTime < as.POSIXct('2021-10-22 14:28:00', tz="UTC"))


#additional cleaning because had to take scan out of water at times
SCAN_Fall <- SCAN_Fall%>% 
  filter(DateTime < as.POSIXct('2021-10-20 18:59:00', tz="UTC") |  
           DateTime > as.POSIXct('2021-10-21 08:29:00', tz="UTC"))%>% 
  filter(DateTime < as.POSIXct('2021-10-21 11:27:00', tz="UTC") |  
           DateTime > as.POSIXct('2021-10-21 12:00:00', tz="UTC"))%>% 
  filter(DateTime < as.POSIXct('2021-10-21 13:14:00', tz="UTC") |  
           DateTime > as.POSIXct('2021-10-21 13:26:00', tz="UTC"))%>%
  filter(DateTime < as.POSIXct('2021-10-21 13:53:00', tz="UTC") |  
           DateTime > as.POSIXct('2021-10-21 13:59:00', tz="UTC"))%>%  
  filter(DateTime < as.POSIXct('2021-10-21 14:15:00', tz="UTC") |  
           DateTime > as.POSIXct('2021-10-21 14:20:00', tz="UTC"))%>% 
  filter(DateTime < as.POSIXct('2021-10-21 15:03:00', tz="UTC") |  
           DateTime > as.POSIXct('2021-10-21 15:15:00', tz="UTC"))%>% 
  filter(DateTime != as.POSIXct('2021-10-21 15:21:00', tz="UTC"))%>% 
  filter(DateTime < as.POSIXct('2021-10-21 15:32:00', tz="UTC") |  
           DateTime > as.POSIXct('2021-10-21 15:38:00', tz="UTC"))%>% 
  filter(DateTime < as.POSIXct('2021-10-21 16:11:00', tz="UTC") |  
           DateTime > as.POSIXct('2021-10-21 16:13:00', tz="UTC"))%>% 
  filter(DateTime != as.POSIXct('2021-10-21 16:44:00', tz="UTC"))%>% 
  filter(DateTime != as.POSIXct('2021-10-21 16:53:00', tz="UTC"))%>% 
  filter(DateTime < as.POSIXct('2021-10-21 16:59:00', tz="UTC") |  
           DateTime > as.POSIXct('2021-10-21 17:01:00', tz="UTC"))%>% 
  filter(DateTime < as.POSIXct('2021-10-21 17:16:00', tz="UTC") |  
           DateTime > as.POSIXct('2021-10-21 17:22:00', tz="UTC"))%>% 
  filter(DateTime < as.POSIXct('2021-10-21 17:50:00', tz="UTC") |  
           DateTime > as.POSIXct('2021-10-22 09:10:00', tz="UTC"))%>% 
  filter(DateTime != as.POSIXct('2021-10-22 09:56:00', tz="UTC"))%>%   
  filter(DateTime < as.POSIXct('2021-10-22 12:03:00', tz="UTC") |  
           DateTime > as.POSIXct('2021-10-22 12:13:00', tz="UTC"))

#more cleaning of outliers... is there a way to PROVE these are outliers?
SCAN_Fall <- SCAN_Fall%>% 
  filter(DateTime != as.POSIXct('2021-10-20 18:31:00', tz="UTC")) %>% 
  filter(DateTime != as.POSIXct('2021-10-20 18:46:00', tz="UTC")) %>% 
  filter(DateTime != as.POSIXct('2021-10-20 18:52:00', tz="UTC"))

SCAN_Fall <- SCAN_Fall%>% 
  filter(DateTime != as.POSIXct('2021-10-21 09:41:00', tz="UTC")) %>% 
  filter(DateTime != as.POSIXct('2021-10-21 10:07:00', tz="UTC")) %>% 
  filter(DateTime != as.POSIXct('2021-10-21 11:12:00', tz="UTC")) %>% 
#  filter(DateTime != as.POSIXct('2021-10-21 15:09:00', tz="UTC"))%>% 
#  filter(DateTime != as.POSIXct('2021-10-21 15:13:00', tz="UTC")) %>% 
  filter(DateTime < as.POSIXct('2021-10-21 16:23:00', tz="UTC")|
         DateTime > as.POSIXct('2021-10-21 16:26:00', tz="UTC")) %>% 
  filter(DateTime != as.POSIXct('2021-10-21 16:28:00', tz="UTC"))%>% 
  filter(DateTime != as.POSIXct('2021-10-21 15:42:00', tz="UTC")) %>% 
  filter(DateTime != as.POSIXct('2021-10-21 15:43:00', tz="UTC")) %>% 
  filter(DateTime != as.POSIXct('2021-10-21 16:42:00', tz="UTC")) %>% 
  filter(DateTime < as.POSIXct('2021-10-21 17:02:00', tz="UTC")|
           DateTime > as.POSIXct('2021-10-21 17:04:00', tz="UTC"))%>% 
  filter(DateTime != as.POSIXct('2021-10-21 17:29:00', tz="UTC"))%>% 
  filter(DateTime != as.POSIXct('2021-10-21 17:47:00', tz="UTC"))%>% 
  filter(DateTime != as.POSIXct('2021-10-22 10:27:00', tz="UTC"))%>%
  filter(DateTime != as.POSIXct('2021-10-22 11:25:00', tz="UTC"))%>% 
  filter(DateTime != as.POSIXct('2021-10-22 11:28:00', tz="UTC"))%>%
filter(DateTime < as.POSIXct('2021-10-22 12:26:00', tz="UTC")|
         DateTime > as.POSIXct('2021-10-22 12:29:00', tz="UTC"))%>% 
  filter(DateTime != as.POSIXct('2021-10-22 12:49:00', tz="UTC"))%>% 
  filter(DateTime != as.POSIXct('2021-10-22 12:59:00', tz="UTC"))%>% 
  filter(DateTime != as.POSIXct('2021-10-22 13:01:00', tz="UTC"))%>% 
  filter(DateTime != as.POSIXct('2021-10-22 13:07:00', tz="UTC"))%>% 
  filter(DateTime != as.POSIXct('2021-10-22 13:43:00', tz="UTC"))

fig2 <- plot_ly(data = SCAN_Fall , x = ~DateTime, y = ~X205.00)
fig2
fig2 <- plot_ly(data = SCAN_Fall , x = ~DateTime, y = ~X300.00)
fig2

##################
####CALIBRATE#####
##################
WaterSamples <- read.csv(here::here("/FallData/WaterSamples/UNC_11-11-2021.csv"))

colnames(WaterSamples) <- c("SampleName","Cl_mgL","SO4_mgL","Br_mgL","NO3_mgL", "DOC_mgL","TDN_mgL")

WaterSamples$SampleNo <-  as.numeric(sub("(_).*","", WaterSamples$SampleName))
WaterSamples$Date <- as.Date(gsub(".*_(.+)_.*", "\\1", WaterSamples$SampleName), format = "%m-%d-%y")
WaterSamples$Time <- sub(".*_", "\\1", WaterSamples$SampleName)

WaterSamples$DateTime <- as.POSIXct(paste(WaterSamples$Date, WaterSamples$Time), format="%Y-%m-%d %H:%M", tz="UTC")

WaterSamples$DateTime <- round_date(WaterSamples$DateTime, unit = "1 minute")


##join SCAN to water samples
#TheUV spectroscopic methods make use of the strong absorption spectrum of nitrate
#between 200-210 nm. However, ultraviolet light is also strongly absorbed by dissolved organic matter in this wavelength band, and some saline constituents may
#as well cause interference - 

#this is a relationship developed 
  #y (absorbance at 205 nm) = 2.8414~ (absorbance at 300 nm) - 0.0126

#Wang et al (2021) The interference from chromophoric dissolved organic matter (CDOM) UV absorbance
#was reduced according to its exponential relationship between 275 and 295 nm
#UV absorption spectrum of CDOM in seawater fits an exponential function with wavelengths 
#ACDOM(λ) = ACDOM(λ0)eS(λ0−λ) + k

#GUO et al (2020) Various parameters commonly used for water quality detection, 
#such as the spectral absorption range of NO3-N and NO2-N, are 200–250 nm. 
#Organic matter and turbidity are effectively absorbed within the range of 380–750 nm 

df <- left_join(WaterSamples,SCAN_Fall, by="DateTime")

fig1 <- plot_ly(data = SCAN_Fall , x = ~DateTime, y = ~X205.00)
fig1
fig1 <- plot_ly(data = df , x = ~DateTime, y = ~NO3_mgL)
fig1

######

fig1 <- plot_ly(data = df%>%filter(SampleNo > 1 & SampleNo < 12) , x = ~NO3_mgL, y = ~X205.00,
                color=~as.factor(SampleNo)
                )
fig1


fig2 <- plot_ly(data = SCAN_Fall , x = ~DateTime, y = ~X250.00)
fig2
fig2 <- plot_ly(data = df , x = ~DateTime, y = ~DOC_mgL)
fig2
fig2 <- plot_ly(data = df , x = ~DOC_mgL, y = ~X250.00,
                color=~as.factor(SampleNo))
fig2


###try to correct for DOC
#y (absorbance at 205 nm) = 2.8414~ (absorbance at 300 nm) - 0.0126
plot_ly(data = SCAN_Fall , x = ~X205.00, y = ~(X300.00*2.8414 - 0.0126),
        color=~DateTime)



###We need to split up above water treatment plant and below, and also South Ellerbe is on it own
#S.Ellerbe 
SE_SCAN <- SCAN_Fall%>% filter(DateTime < as.POSIXct('2021-10-21 08:30:00', tz="UTC"))
#Upstream: 
Up_SCAN <- SCAN_Fall%>% filter(#DateTime < as.POSIXct('2021-10-21 08:30:00', tz="UTC")|
         DateTime > as.POSIXct('2021-10-21 14:30:00', tz="UTC"))
Up_SCAN <- left_join(Up_SCAN,WaterSamples, by="DateTime")
#Downstream
Down_SCAN <- SCAN_Fall%>%filter(DateTime > as.POSIXct('2021-10-21 08:30:00', tz="UTC")&
           DateTime < as.POSIXct('2021-10-21 14:30:00', tz="UTC"))
Down_SCAN <- left_join(Down_SCAN,WaterSamples, by="DateTime")

#figure out DOC
fig2 <- plot_ly(data = Down_SCAN , x = ~DOC_mgL, y = ~X210.00)
fig2
fig2 <- plot_ly(data = Down_SCAN , x = ~DOC_mgL, y = ~X250.00)
fig2
fig2 <- plot_ly(data = Down_SCAN , x = ~DOC_mgL, y = ~X300.00,
                color=~as.factor(SampleNo))
fig2

plot_ly(data = Down_SCAN#%>%filter(SampleNo!=1) 
        , x = ~X205.00, y = ~(X300.00*2.8414 - 0.0126))
plot_ly(data = Down_SCAN%>%filter(SampleNo!=1) 
        , x = ~DOC_mgL, y = ~(X300.00*2.8414 - 0.0126))
fig2 <- plot_ly(data = Down_SCAN %>%filter(SampleNo!=1), x = ~NO3_mgL, y = ~(X205.00-(X300.00*2.8414 - 0.0126)), color = ~SampleNo)
fig2
fig2 <- plot_ly(data = Down_SCAN , x = ~NO3_mgL, y = ~X205.00)
fig2


### sample no 1 is also its own thing! yikessss
#best we have, for upstream of water treatment plant (not including S Ellerbe)
Up_SCAN$formula <- Up_SCAN$X205.00-(Up_SCAN$X300.00*2.8414 - 0.0126)
plot(NO3_mgL ~ formula, data = Up_SCAN%>%filter(SampleNo!=1))
NO3_model <- lm(NO3_mgL ~ formula 
              #  + X200.00 
                + X205.00 
              #  + X207.50
                + X210.00,
                data = Up_SCAN %>%filter(SampleNo!=1))
summary(NO3_model)

  # NO3_mgL = (X205.00-(X300.00*2.8414 - 0.0126))

##DOC upstream
DOC_model <- lm(DOC_mgL ~ X210.00 + X250.00 + X300.00, data = Up_SCAN%>%filter(SampleNo!=1))
summary(DOC_model)

##NO3 downstream
Down_SCAN$formula <- Down_SCAN$X205.00-(Down_SCAN$X300.00*2.8414 - 0.0126)
plot(NO3_mgL ~ formula, data = Down_SCAN%>%filter(SampleNo!=1))
NO3_model <- lm(NO3_mgL ~ formula 
                #  + X200.00 
                + X205.00 
                #  + X207.50
                + X210.00,
                data = Down_SCAN %>%filter(SampleNo!=1))
summary(NO3_model)

###DOC downstream
DOC_model <- lm(DOC_mgL ~ X210.00 + X250.00 + X300.00, data = Down_SCAN)
summary(DOC_model)
################################
#EQUATIONS FOR NITRATE AND DOC###
################################
##UPSTREAM 
#DOC_mgL =  -3.7084 + X210.00*-0.1675 + X250.00*0.7218  + X300.00*-0.5810
#Multiple R-squared:  0.8067, p-value: 0.003161
##UPSTREAM 
#NO3_mgL =  1.043874 + formula*0.009735 + X205.00*-0.039716 + X210.00*0.046326  
#Multiple R-squared:  0.5265, p-value: 0.0973
Up_SCAN$NO3_predict <-  1.043874 + Up_SCAN$formula*0.009735 + Up_SCAN$X205.00*-0.039716 + Up_SCAN$X210.00*0.046326  
Up_SCAN$DOC_predict <- -3.7084 + Up_SCAN$X210.00*-0.1675 + Up_SCAN$X250.00*0.7218  + Up_SCAN$X300.00*-0.5810

##Downstream 
#NO3_mgL =  3.30810 + formula*0.02228  + X205.00*-0.08567  + X210.00*0.09697  
#Multiple R-squared:  0.9988, p-value: 0.001793
Down_SCAN$NO3_predict <- 3.30810 + Down_SCAN$formula*0.02228  + Down_SCAN$X205.00*-0.08567  + Down_SCAN$X210.00*0.09697
##Downstream
#DOC_mgL =  15.83120 + X210.00*0.01508  + X250.00*0.10726  + X300.00*-0.20289 
#Multiple R-squared:  0.9247, p-value: 0.1108
Down_SCAN$DOC_predict <- 15.83120 + Down_SCAN$X210.00*0.01508  + Down_SCAN$X250.00*0.10726  + Down_SCAN$X300.00*-0.20289 

Mainstem_SCAN <- rbind(Down_SCAN,Up_SCAN)

#write progress out
write.csv(Mainstem_SCAN,here::here("FallData/Mainstem_SCAN_predict_2022-01-22.csv"),row.names = FALSE)

#plot
fig <- plot_ly(data = Mainstem_SCAN , x = ~DateTime, y = ~NO3_predict)
fig <- plot_ly(data = df, x = ~DateTime, y = ~SO4_mgL)
fig <- plot_ly(data = df, x = ~DateTime, y = ~Br_mgL)
fig <- plot_ly(data = df, x = ~DateTime, y = ~NO3_mgL)
fig <- plot_ly(data = df, x = ~DateTime, y = ~DOC_mgL)
fig <- plot_ly(data = df, x = ~DateTime, y = ~TDN_mgL)

fig

fig2 <- plot_ly(data = df , x = ~DateTime, y = ~X210.00)
fig2 <- plot_ly(data = SCAN_Fall , x = ~DateTime, y = ~X205.00)

fig2 <- plot_ly(data = df, x = ~DateTime, y = ~X255.00)
fig2 <- plot_ly(data = df, x = ~DateTime, y = ~Br_mgL)
fig2 <- plot_ly(data = df, x = ~DateTime, y = ~NO3_mgL)
fig2 <- plot_ly(data = df, x = ~DateTime, y = ~DOC_mgL)
fig2 <- plot_ly(data = df, x = ~DateTime, y = ~TDN_mgL)

fig2

ggplot(data=SCAN_Fall) + geom_point(aes(DateTime,X205.00))
hist(SCAN_Fall$X205.00)
## now let's see if claibrations of two points hold for point in the middle
plot(df$X205.00~df$X300.00)
plot(df$X300.00~df$DOC_mgL)
plot(df$X300.00~df$DOC_mgL)
plot(df$X300.00~df$DOC_mgL)



df_calibration1 <- df%>%filter(SampleNo >= 2 & SampleNo <= 4)

#plot(df_calibration1$X200.00~df_calibration1$NO3_mgL)
plot(df_calibration1$X205.00~df_calibration1$NO3_mgL)
#plot(df_calibration1$X275.00~df_calibration1$NO3_mgL)
plot(df_calibration1$X205.00~df_calibration1$X300.00)

df_calibration2 <- df%>%filter(SampleNo >= 2 & SampleNo <= 6)

plot(df_calibration2$X205.00~df_calibration2$NO3_mgL)
plot(df_calibration2$DOC_mgL~df_calibration2$DOC_mgL)

#plot(df_calibration2$X200.00~df_calibration2$NO3_mgL)
#plot(df_calibration2$X202.50~df_calibration2$NO3_mgL)
#plot(df_calibration2$X207.50~df_calibration2$NO3_mgL)
#plot(df_calibration2$X275.00~df_calibration2$NO3_mgL)
#plot(df_calibration2$X205.00~df_calibration2$X300.00)

df_calibration3 <- df%>%filter(SampleNo ==  1 | SampleNo >= 7)

plot(df_calibration3$X200.00~df_calibration3$NO3_mgL)
plot(df_calibration3$X202.50~df_calibration3$NO3_mgL)
plot(df_calibration3$X205.00~df_calibration3$NO3_mgL)
plot(df_calibration3$X207.50~df_calibration3$NO3_mgL)
plot(df_calibration3$X210.00~df_calibration3$NO3_mgL)


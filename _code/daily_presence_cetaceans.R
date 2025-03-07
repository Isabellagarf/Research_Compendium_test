# Simplified script to make the SNE baseline daily presence plot for all cetaceans
# based on code from: GE Davis 02/10/2023, adapted from Davis et al 2020 5 species presence plot


#### LOAD REQUIRED PACKAGES #####

library(tidyverse)
library(lubridate)
library(tsibble)
#this is an edit

#### READ IN COMPILED SAVED DATA ####
all_sp <- read.csv("_data/all_sp_dataframe.csv")

# the benefit of an R project is that your working directory is automatically set to the repo folder
# this way you don't have to use full paths (like below) that are specific to your computer and can more easily collaborate with others
# all_sp <- read.csv("C:/Users/rebecca.vanhoeck/Documents/GitHub/researchCompendium/data/all_sp_dataframe.csv")


#### WRANGLE DATA ####

#filter out data that is outside of recording bounds
allsp_forplot <- all_sp %>% 
  mutate_at(c("StartDate", "DataStart", "DataEnd"), ymd) %>% 
  group_by(PROJECT_DESCRIPTION) %>%  filter(StartDate >= DataStart & StartDate <= DataEnd)

# Vector to find all missing dates in data from detection data
all_dates <- allsp_forplot %>% ungroup() %>%  select(StartDate, SITE_NAME) %>%  distinct() %>% as_tsibble(key=SITE_NAME)
#find missing weeks within whole data range, for each station
missing_dates <- count_gaps(all_dates, .full=TRUE) 



#### Summarize 1 year total daily presence summary across Jan-Dec ####

#format dates to make all same date but have just month-day
allsp_forplot$mday <- as.Date(paste(month(allsp_forplot$StartDate), day(allsp_forplot$StartDate), sep = "-"), format= "%m-%d")

#remove the one leap year day (1 day has no true detections) for plotting
allsp_forplot_noFeb29 <- allsp_forplot %>% filter(!is.na(mday))

#get FPOD missing dates
# Vector to find all missing dates in data from detection data
all_datesporp <- allsp_forplot_noFeb29 %>%  filter(!is.na(Porpoise_Occur)) %>% ungroup() %>%  select(SITE_NAME, mday) %>%  distinct() %>% as_tsibble(index= mday, key=SITE_NAME)
#find missing weeks within whole data range, for each station
missing_dates_porp <- count_gaps(all_datesporp, .full=TRUE) 

#create missing FPOD dates manually
missing_dates_porp2 <- data.frame(SITE_NAME = c("NS01", "NS03", "NS04", "NS05"), 
                                 .from = ymd(c("2023-05-30", "2023-02-06", "2023-02-06", "2023-02-06" )), 
                                 .to = ymd(c("2023-10-12", "2023-05-30", "2023-05-29", "2023-05-29")))

# Vector to find all missing dates in data from detection data
all_dates2 <- allsp_forplot_noFeb29 %>%  ungroup() %>%  select(SITE_NAME, mday) %>%  distinct() %>% as_tsibble(index= mday, key=SITE_NAME)
#find missing weeks within whole data range, for each station
missing_dates_yr <- count_gaps(all_dates2, .full=TRUE) 


#### PLOT ALL SPECIES #####
ggplot(data= allsp_forplot_noFeb29) + 
  facet_grid((factor(SITE_NAME, levels=c( "COX01","COX02", "NS01","NS02", "NS03", "NS04",  "NS05")))~., drop=FALSE, switch="y") +
  geom_rect(data = (allsp_forplot%>% filter(Porpoise_Occur==1)), aes(xmin=mday-0.5,xmax=mday+0.5, ymin=3.4, ymax=4.0, fill="Harbour Porpoise"), linetype="blank") + 
  geom_rect(data = (missing_dates_porp2), aes(xmin = .from, xmax = .to, ymin = 3.3, ymax = 4.2), 
            colour = "grey100", linetype = "blank", alpha = 0.5, show.legend = NA) +
  
  geom_rect(data=(allsp_forplot%>% filter(Dolphin_Occur==1)), aes(xmin=mday-0.5,xmax=mday+0.5, ymin=2.6, ymax=3.2, fill="Delphinid spp."), linetype="blank") +
  
  geom_rect(data=(allsp_forplot%>% filter(Sperm_Occur==1)), aes(xmin=mday-0.5,xmax=mday+0.5, ymin=1.8, ymax=2.4, fill="Sperm"), linetype="blank") +
  
  geom_rect(data=(allsp_forplot%>% filter(Humpback_Occur==1)), aes(xmin=mday-0.5,xmax=mday+0.5, ymin=1.0, ymax=1.6, fill="Humpback"), linetype="blank") +
  
  geom_rect(data=(allsp_forplot%>% filter(Minke_Occur==1)), aes(xmin= mday-0.5 , xmax=mday+0.5, ymin=0.2, ymax=0.8, fill="Minke"), linetype="blank") +
  
  geom_rect(data=(allsp_forplot%>% filter(NARW_Occur==1)), aes(xmin= mday-0.5 , xmax=mday+0.5, ymin=-0.6, ymax=0, fill="NARW"), linetype="blank") +
  
  geom_rect(data=(allsp_forplot%>% filter(Sei_Occur==1)), aes(xmin= mday-0.5 , xmax=mday+0.5, ymin=-1.4, ymax=-0.8, fill="Sei"), linetype="blank") +
  
  geom_rect(data=(allsp_forplot%>% filter(Fin_Occur==1)), aes(xmin= mday-0.5 , xmax=mday+0.5, ymin=-2.2, ymax=-1.6, fill="Fin"), linetype="blank") +
  
  geom_rect(data=(allsp_forplot%>% filter(Blue_Occur==1)), aes(xmin= mday-0.5 , xmax=mday+0.5, ymin=-3, ymax=-2.4, fill="Blue"), linetype="blank") +
  #missing data
  geom_rect(data = missing_dates_yr, 
            aes(xmin = .from, xmax = .to, ymin = -Inf, ymax = Inf), 
            colour = "grey100", linetype = "blank", alpha = 0.5, show.legend = NA) +
 
 scale_x_date(limits= as.Date(c("01-01", "12-31"), format="%m-%d"), expand = c(-.00002, .9999999), date_breaks = "1 month", date_labels = "%b") +
  ggtitle("All Species Daily Presence Summaries (2004-2014)")+
  theme_bw()+
  theme(text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) + #remove y axis values
  ylab("Site")+
  #legend 
  scale_fill_manual(values=c("NARW" = "#E69F00",
                             "Humpback"= "#CC79A7",
                             "Sei"= "#800000", 
                             "Fin" = "#009E73",
                             "Blue"= "#08519c", 
                             "Minke" ="#F94144" ,
                             "Sperm" ="#0f4c5c" , 
                             "Harbour Porpoise" ="black", 
                             "Delphinid spp." = "#00afb9"), 
                    name="Species",
                    breaks=c("Harbour Porpoise","Delphinid spp.","Sperm", "Humpback","Minke", "NARW", "Sei", "Fin", "Blue"))



# Save plot
ggsave("_figs/dailyPresenceAllCetatceans.png", width=10, height=8, units="in", dpi=1200)


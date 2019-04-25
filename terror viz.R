# Loac packages

library(rvest)
library(tidyverse)
library(ggplot2)
library(lubridate)

# Scraping

GTD1 <- read_html("https://www.start.umd.edu/gtd/search/Results.aspx?page=1&casualties_type=b&casualties_max=&start_yearonly=2012&end_yearonly=2017&dtp2=all&country=217&count=100&expanded=no&charttype=line&chart=overtime&ob=GTDID&od=desc#results-table")

terror1 <- GTD1 %>%
  html_nodes("#results-table") %>%
  html_table(header = TRUE)
terror1 <- terror1[[1]]

GTD2 <- read_html("https://www.start.umd.edu/gtd/search/Results.aspx?page=2&casualties_type=b&casualties_max=&start_yearonly=2012&end_yearonly=2017&dtp2=all&country=217&count=100&expanded=no&charttype=line&chart=overtime&ob=GTDID&od=desc#results-table")

terror2 <- GTD2 %>%
  html_nodes("#results-table") %>%
  html_table(header = TRUE)
terror2 <- terror2[[1]]

GTD3 <- read_html("https://www.start.umd.edu/gtd/search/Results.aspx?page=3&casualties_type=b&casualties_max=&start_yearonly=2012&end_yearonly=2017&dtp2=all&country=217&count=100&expanded=no&charttype=line&chart=overtime&ob=GTDID&od=desc#results-table")

terror3 <- GTD3 %>%
  html_nodes("#results-table") %>%
  html_table(header = TRUE)
terror3 <- terror3[[1]]

terror <- rbind(terror1, terror2, terror3)

terror$Group[terror$`PERPETRATOR GROUP` == "Anti-Abortion extremists" | 
               terror$`PERPETRATOR GROUP` == "Anti-Arab extremists" | 
               terror$`PERPETRATOR GROUP` == "Anti-Government extremists" | 
               terror$`PERPETRATOR GROUP` == "Anti-Gun Control extremists" | 
               terror$`PERPETRATOR GROUP` == "Anti-LGBT extremists" | 
               terror$`PERPETRATOR GROUP` == "Anti-Muslim extremists" | 
               terror$`PERPETRATOR GROUP` == "Anti-Muslim extremists (suspected)" | 
               terror$`PERPETRATOR GROUP` == "Anti-Semitic extremists" | 
               terror$`PERPETRATOR GROUP` == "Anti-Sikh extremists" | 
               terror$`PERPETRATOR GROUP` == "Citizens for Constitutional Freedom" | 
               terror$`PERPETRATOR GROUP` == "Incel extremists" | 
               terror$`PERPETRATOR GROUP` == "Ku Klux Klan (suspected)" | 
               terror$`PERPETRATOR GROUP` == "Neo-Nazi extremists" | 
               terror$`PERPETRATOR GROUP` == "Right-wing extremists" | 
               terror$`PERPETRATOR GROUP` == "Neo-Nazi extremists" | 
               terror$`PERPETRATOR GROUP` == "Sovereign Citizen" | 
               terror$`PERPETRATOR GROUP` == "Sovereign Citizen (suspected)" | 
               terror$`PERPETRATOR GROUP` == "Sovereign Citizen,White extremists" | 
               terror$`PERPETRATOR GROUP` == "United Aryan Empire" | 
               terror$`PERPETRATOR GROUP` == "White extremists" | 
               terror$`PERPETRATOR GROUP` == "White extremists (suspected)" | 
               terror$`PERPETRATOR GROUP` == "White Rabbit Three Percent Illinois Patriot Freedom Fighters Militia"] <- "Far-right"

terror$Group[terror$`PERPETRATOR GROUP` == "Unknown"] <- "Unknown"

terror$Group[is.na(terror$Group)] <- "Other"

terror$DATE <- as.Date(terror$DATE)

terror$year <- year(terror$DATE)

aggregate(terror$FATALITIES, by = list(Category = terror$year, terror$Group), FUN = sum) -> agg


spread(agg, Group.2, x) -> agg

as.data.frame(table(terror$year, terror$Group)) -> attacks

spread(attacks, Var2, Freq) -> attacks

agg$ratio <- agg$`Far-right`/agg$Other

attacks$ratio <- attacks$`Far-right`/attacks$Other

  
# Viz

as.data.frame(table(terror$year, terror$Group)) -> plot.dta
plot.dta <- plot.dta[plot.dta$Var1 == 2017, ]

plot <- ggplot(data = plot.dta, aes(x = Var2, y = Freq, fill = Var2)) + 
  geom_bar(stat = "identity") + theme_minimal() + 
  scale_fill_manual(values=c("#FD0000", "#00A01E", "#004FFF")) + 
  ggtitle("U.S. Terror Attacks in 2017 by Perpetrator Group Category") + 
  xlab("Category") + ylab("Attacks") + theme(legend.position="none")

fatal <- agg[agg$Category == 2017, ]
fatal <- gather(fatal, `Far-right`, `Other`, `Unknown`, key = "group", value = "fatalities")


plot <- ggplot(data = fatal, aes(x = group, y = fatalities, fill = group)) + 
  geom_bar(stat = "identity") + theme_minimal() + 
  scale_fill_manual(values=c("#FD0000", "#00A01E", "#004FFF")) + 
  ggtitle("U.S. Terror Fatalities in 2017 by Perpetrator Group Category") + 
  xlab("Category") + ylab("Fatalities") + theme(legend.position="none")

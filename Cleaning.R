library (plyr)
library (dplyr)
library (readxl)
library (highcharter)
library (dygraphs)
library (lubridate)
library (stringr)
library(ggplot2)
library(viridis)
library(ggExtra)
library (esquisse)


invoice <- read_excel("morazan.xls", sheet = "HInvoice")
clientes <- read_excel("morazan.xls", sheet = "Cliente")

#Unir tablas
merged <- left_join (invoice, clientes, by = "CliID")

### Estandarizar CliID ###

merged$CliID <- 
  str_remove_all(merged$CliID, "[-_ N=.a-zA-Z]") %>%
  trimws() %>%
  stringr::str_replace('\\*', "")
  
  
merged$CliID <- ifelse(
                  startsWith(merged$CliID, "9"),
                  yes = "contado",
                  no = merged$CliID)


####################################
### Data Preparation For Graphs ###
###################################

## Polar Graph Data

polardata <- 
  select(merged,
         InvoiceNumber,
         stamp = SysDate,
         CliID,
         CliName,
         VentaTotal) %>%
  mutate(hour = hour(stamp),
         weekday = weekdays(stamp),
         monthday = mday(stamp),
         month = month(stamp),
         year = year(stamp))

## Hourly Heatmap Data

hourlyheatmapdata <- 
  group_by(polardata, hour, weekday, monthday, month, year) %>%
  summarise(despachos = n(),
            VentaTotal = sum(VentaTotal))

## Weekly Heatmap Data

weeklyheatmapdata <- 
  group_by(polardata, weekday, monthday, month, year) %>%
  summarise(despachos = n(),
            VentaTotal = sum(VentaTotal))



###################################
##### Grafico de Flujo Horario ####
###################################

## All Hourly Graph ##

dotplot <- 
  hourlyheatmapdata %>%
  filter(despachos >= 1L & despachos <= 100L) %>%
  filter(VentaTotal >= 
           0 & VentaTotal <= 60000) %>%
  filter(hour > 4) %>%
  ggplot() +
  aes(x = hour, y = despachos, colour = (-1*despachos)) +
  geom_jitter(shape = 16, size = 5, width = 0.3, alpha = 0.5) +
  geom_smooth(method = "loess", span = 0.6) +
  scale_color_viridis_c(option = "viridis") +
  theme_minimal() 

## Weekday Day-Hourly Graph

dotplot <- 
  weeklyheatmapdata %>%
  filter(despachos >= 1 & despachos < 1100) %>%
  #filter(VentaTotal >= 
   #        0 & VentaTotal <= 60000) %>%
  #filter(hour > 4) %>%
  ggplot() +
  aes(x = weekday, y = despachos, colour = (-1*despachos)) +
  geom_jitter(shape = 16, size = 5, width = 0.4, alpha = 0.5) +
  geom_smooth() +
  scale_color_viridis_c(option = "viridis") +
  theme_minimal() 

### Weekday DotPlot

hourlyheatmapdata %>%
  filter(despachos >= 1 & despachos <= 200) %>%
  #filter(VentaTotal >= 
   #        0 & VentaTotal <= 60000) %>%
  filter(hour > 4) %>%
  ggplot() +
  aes(x = hour, y = despachos, colour = (-1*despachos)) +
  geom_jitter(shape = 16, size = 5, width = 0.3, alpha = 0.5) +
  geom_smooth(method = "loess", span = 0.2) +
  scale_color_viridis_c(option = "viridis") +
  theme_minimal() +
  facet_wrap(~ weekday, ncol=3)


### Weekday Barplot
weekbarplot <- 
  select(weeklyheatmapdata, weekday, despachos) %>%
  group_by(weekday) %>%
  summarise(sumdespachos = sum(despachos))

ggplot(weekbarplot, aes(x=weekday, y=sumdespachos)) + geom_bar ()




#####################
### Grafico Polar ###
#####################


polardata <- 
  select(merged,
         InvoiceNumber,
         stamp = SysDate,
         CliID,
         CliName,
         VentaTotal) %>%
  mutate(hour = hour(stamp),
         weekday = weekdays(stamp),
         monthday = mday(stamp),
         month = month(stamp),
         year = year(stamp))


# Chrono-binned data
heatmapdata <- 
  group_by(polardata, hour, monthday, month, year) %>%
  summarise(despachos = n(),
            VentaTotal = sum(VentaTotal))

heatmapdata$despachos <- ifelse(heatmapdata$despachos > 200,
                                yes = 200,
                                no = heatmapdata$despachos)

  
  
  






#Start Polar Graph

polargraph <-
  ggplot(polardata, aes(x=hour, fill=VentaTotal)) + 
  geom_histogram(breaks = seq(0, 24), colour = "grey") + 
  coord_polar(start = 0) + 
  theme_minimal() + 
  scale_fill_brewer() + 
  ylab ("Despachos") + 
  ggtitle ("Despachos por hora del Dia") + 
  scale_x_continuous("", limits = c(0,24), breaks = seq(0,24),
                     labels = seq(0,24))

########################
# Dataset for Heatmap #
#######################



### Start Heatmap ####
#Not very insightful with current data and range of dispatch


heatmap <-
  ggplot(heatmapdata, aes(monthday, hour, fill=despachos))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="Despachos por Hora",option ="C") +
  facet_grid(year~month) + 
  scale_y_continuous(trans = "reverse", breaks = unique(heatmapdata$hour)) + 
  scale_x_continuous(breaks =c(1, 15, 30)) + 
  theme_minimal(base_size = 8)

p <-p + labs(title= paste("Hourly Temps - Station",statno), x="Day", y="Hour Commencing")
p <-p + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra
























### Encontrar frecuencia por cliente###

clientfreq <- 
  group_by(merged, CliName, CliID, CliCodigo) %>%
  summarise(freq = n())


  








codesum <- group_by(merged,
                    CliCodigo) %>%
  summarise(freq = n())


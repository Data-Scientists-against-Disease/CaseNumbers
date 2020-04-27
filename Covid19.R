library(tidyverse)
library(readr)

time_series_covid19_confirmed_global <- read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
#View(time_series_covid19_confirmed_global)

covid = time_series_covid19_confirmed_global

l=length(covid)
covid$wachstumAlt=(covid[,l-5]/covid[,l-10])^0.2
covid$wachstum=(covid[,l]/covid[,l-5])^0.2
covid$change=(covid$wachstum-covid$wachstumAlt)
covid$einMonat=covid[,l]*covid$wachstum^30
covid$zweiMonate=covid$einMonat*(covid$wachstum/2+0.5)^30


View(covid[covid[,l]>1000&is.na(covid$`Province/State`),c(1:2,l:(length(covid)))])


#Einbauen: Anzahl Corona-Tests berücksichtigen
#Testing einlesen
#Vergleich Testing-Anzahl zu Fallzahl



# Tabelle anlegen mit Daten zu Staaten, diese mischen. Evtl. gibts die Tabelle auch bei den WirVsVirus Daten
# Bevölkerungszahl insgesamt; BIP/EInwohner; (Alterspyramide?); Staatsoberhaupt männlich/weiblich; Staatsform; Demokratieindex

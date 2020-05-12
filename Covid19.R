library(tidyverse)
library(readr)

f = function(dset)
{
  dset$wachstumAlt=(dset[,l-5]/dset[,l-10])^0.2
  dset$wachstum=(dset[,l]/dset[,l-5])^0.2
  dset$change=(dset$wachstum-dset$wachstumAlt)
  dset$einMonat=dset[,l]*dset$wachstum^30
  dset$zweiMonate=dset$einMonat*(dset$wachstum/2+0.5)^30
  return(dset)
}

display = function(dset)
{
  dset$wachstum=round(dset$wachstum*100-100,2)
  dset$wachstumAlt=round(dset$wachstumAlt*100-100,2)
  dset$change=(dset$wachstum-dset$wachstumAlt)
  dset$einMonat=round(dset$einMonat,0)
  dset$zweiMonate=round(dset$zweiMonate,0)
  
  View(dset[dset[,l]>1000&is.na(dset$`Province/State`),c(1:2,l:(length(dset)))])
}

time_series_covid19_recovered_global <- read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
#View(time_series_covid19_recovered_global)
recovered = f(time_series_covid19_recovered_global)

time_series_covid19_confirmed_global <- read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
View(time_series_covid19_confirmed_global)
covid = time_series_covid19_confirmed_global
cases = f(covid)
cases = merge(recovered[1:2], cases, by=c("Province/State","Country/Region"))

time_series_covid19_deaths_global <- read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
#View(time_series_covid19_deaths_global)
deaths = f(time_series_covid19_deaths_global)
deaths = merge(recovered[1:2], deaths, by=c("Province/State","Country/Region"))

recovered = merge(deaths[1:2], recovered, by=c("Province/State","Country/Region"))

display(cases)

active[,1:4] = cases[,1:4]
active[,5:l] = cases[,5:l] - deaths[,5:l] - recovered[,5:l]
active = f(active)

display(active)

# Prognose mit folgenden Korrekturen der Zeitlinie erweitern:
# Fälle haben sich vor n Tagen angesteckt
# Recovered haben sich vor r Tagen angesteckt
# Deaths haben sich vor d Tagen angesteckt

#Einbauen: Anzahl Corona-Tests berücksichtigen
#Testing einlesen
#Vergleich Testing-Anzahl zu Fallzahl



# Tabelle anlegen mit Daten zu Staaten, diese mischen. Evtl. gibts die Tabelle auch bei den WirVsVirus Daten
# Bevölkerungszahl insgesamt; BIP/EInwohner; (Alterspyramide?); Staatsoberhaupt männlich/weiblich; Staatsform; Demokratieindex

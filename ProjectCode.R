#-----------Input Excel Data for emissions---------------------
library("readxl")

emissions <- read_excel("~/DATA 375/Project/Emissions.xlsx")

year <- emissions$Year
transportation <- emissions$Transportation
electricity <- emissions$'Electric Power'
commercial_residential <- emissions$'Commercial and Residential'
agriculture <- emissions$Agriculture
biogenic <- emissions$'CO2 from Biogenic Sources'
excluded <- emissions$'Excluded Emissions'
total_emissions <- emissions$'Total CO2 Emssions'

#------------Plot Trend of Emissions over time-------------------

par(mfrow=c(1,2))
plot(year,transportation,lwd=2, ylab = 'Emissions due to Transoportation (million tonnes)',xlab = 'Year',
     main='California Transportation Emissions 2002-2017')
plot(year,electricity,lwd=2, ylab = 'Emissions due to Electricity (million tonnes)',xlab = 'Year',
     main='California Electricity Emissions 2002-2017')

par(mfrow=c(1,2))
plot(year,commercial_residential,lwd=2, ylab = 'Emissions due to Commercial and Residential Emissions (million tonnes)',
     xlab = 'Year',main='California Commercial and Residential Emissions 2002-2017')
plot(year,agriculture,lwd=2, ylab = 'Emissions due to Agriculture (million tonnes)',xlab = 'Year',
     main='California Agricultural Emissions 2002-2017')

par(mfrow=c(1,2))
plot(year,biogenic,lwd=2, ylab = 'Emissions due to Biogenic (million tonnes)',xlab = 'Year',
     main='California Biogenic Emissions 2002-2017')
plot(year,excluded,lwd=2, ylab = 'Emissions Excluded (million tonnes)',xlab = 'Year',
     main='California Excluded Emissions 2002-2017')

plot(year,total_emissions,lwd=2, ylab = 'Total Emissions (million tonnes)',xlab = 'Year',
     main="Californias Total Emissions 2002-2017")

#------------Input Excel Data for Wildfires-------------------------

wildfires <- read_excel("~/DATA 375/Project/Wildfires.xlsx")

fires <- wildfires$'Number of Fires'
acres <- wildfires$'Number of Acres'

#------------Plot Trend Of Fires------------------------------------

par(mfrow=c(1,2))
plot(year,fires,lwd=2, ylab = 'Total Number of Fires',xlab = 'Year',
     main='Fires in California 2002-2017')
plot(year,acres,lwd=2, ylab = 'Total Number of Acres Destroyed',xlab = 'Year',
     main='Acres Destroyed in California 2002-2017')

#------------Input Excel Data for Population-------------------------

population <- read_excel("~/DATA 375/Project/Population.xlsx")

growth <- population$'Population in millions'

#------------Plot Trend of Population-------------------------------
plot(year,growth,lwd=2, ylab = 'Total Number of People (millions)',xlab = 'Year',
     main='California Population 2002-2017')

#-----------Correlations-------------------------------------------
pop_fires <- cbind(growth,fires)
cor(pop_fires)

pop_acres <- cbind(growth, acres)
cor(pop_acres)

pop_emiss <- cbind(growth,total_emissions)
cor(pop_emiss)

emiss_fires <- cbind(total_emissions,fires)
cor(emiss_fires)

emiss_acres <- cbind(total_emissions,acres)
cor(emiss_acres)

#-----------Plot variables-------------------------------------------
plot(fires,growth,main = "Total fires and Population")
plot(acres,growth, main = 'Acres Destroyed and Population')
plot(growth,total_emissions,main = 'Population and Total Carbon Emissions')
plot(fires,total_emissions,main = 'Total Fires and Total Carbon Emissions')
plot(acres,total_emissions,main = 'Acres Destroyed and Total Carbon Emissions')



summary(lm(fires~acres+agriculture+biogenic+commercial_residential+electricity+excluded+transportation))
plot(lm(fires~acres+agriculture+biogenic+commercial_residential+electricity+excluded+transportation))
if(!require(dplyr)){install.packages("dplyr")}
if(!require(COVID19)){install.packages("COVID19")}

CumulativeCovidDeathByCountry <- function(data,
                                          country="DEU",
                                          influenza_deaths=10,
                                          upper_limit=40) {
  #extract the country's data subset from the world data set
  thiscountry <- data[data$id==country,]
  #select data from the first reported Covid-19 death onwards
  CovidDeaths  <- subset(thiscountry, deaths>0)
  #calculate new daily deaths from the published cumulative deaths
  CovidDeaths$new_deaths <- lead(CovidDeaths$deaths) - CovidDeaths$deaths
  #scale deaths / 100,000 population
  popfactor = CovidDeaths$pop/100000
  CovidDeaths$deaths100k <- CovidDeaths$deaths/popfactor
  CovidDeaths$new_deaths100k <- CovidDeaths$new_deaths/popfactor

  #plot the cumulative deaths
  barplot(CovidDeaths$deaths/popfactor, ylim=c(0,upper_limit),
          width=3, col="blue",
          main=paste("Covid-19 reported deaths per 100,000 pop. in",
CovidDeaths$country[1], " "),
          xlab=paste(nrow(CovidDeaths), "days since first reported death", " "),
          ylab="deaths per 100,000" )

  #superimpose the daily reported new deaths
  par(new = T)
  barplot(CovidDeaths$new_deaths/popfactor, ylim=c(0,upper_limit),
          width=3, col="green")


#indicate the number of seasonal RTI deaths in the past year
  abline(h=influenza_deaths, col="red")
}

world <- covid19()

CumulativeCovidDeathByCountry(world, "DEU")

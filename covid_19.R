setwd("~/Secretaría de Finanzas/Cursos Udemy/Introducción Data Science")
rm(list = ls())
options(scipen=999)

#install.packages("lubridate")
#install.packages("gifski")
#install.packages("av")

library(jsonlite)
library(curl)
library(lubridate)
library(ggplot2)
library(gganimate)
library(tidyr)

api_covid <- "http://api.covid19api.com/dayone/country/mexico"
data_covid <- fromJSON(api_covid)
data_covid$Date <- date(data_covid$Date)
data_covid <- (data_covid[, c(1,8:12)])

data_covid_2 <- gather(data_covid, "Tipo", "Casos", 2:5, na.rm = TRUE)

ggplot(data_covid_2, aes(x=Date, y=Casos))+
  geom_line(aes(color = factor(Tipo)), size =2)+
  labs(title = "Casos de covid-19 en México el día {frame_along}",
      x = "Fecha",
      y = "Casos")+
      theme(legend.position = "top")+
      theme(legend.title = element_blank())+
      scale_color_manual(values = c("blue", "red", "black","green"))+
      transition_reveal(Date)


####################################################################################

api_population <- "http://api.worldbank.org/v2/country/all/indicator/SP.POP.TOTL?format=json&per_page=20000"
data_population <- fromJSON(api_population)

library(plyr)
library(dplyr)

population <- rbind.fill(lapply(data_population,
                                function(x) do.call("data.frame", as.list(x))
                                ))

population <- population[-1,]
population <- population[,c(8,10,12,13)]
population <- population[complete.cases(population),]
population <- population[-c(1:2869),]
colnames(population) <- c("Indicador", "País", "Año", "Habitantes")
population$Año <- as.numeric(population$Año)

ggplot(population, aes(x=reorder(x=País,+Habitantes), y=Habitantes))+
  geom_bar(stat="identity")+
  coord_flip(xlim = c(length(unique(population$País))-19,
                      length(unique(population$País))))+
  labs(title = "Población en el año {population$Año[frame_along]}",
       x = "País",
       y = "Habitantes")+
  transition_reveal(Año)

############################################################################
ggplot(mtcars, aes(x=wt, y=mpg, color=disp))+
  geom_point()+
  geom_smooth()+
  labs(title="Consumo combustible")

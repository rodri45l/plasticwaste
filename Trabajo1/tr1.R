library(tidyverse)
library(rio)
library(plotly)
df1<- read_csv("./Datos/nation_1751_2017.csv")
df2 <- df1[-c(1:4), ]
df2[ df2 == "." ] <- NA
 df3 <-  transform(df2, X2 = as.numeric(X2),
   X3 = as.numeric(X3),
   X4 = as.numeric(X4),
   X5 = as.numeric(X5),
   X6 = as.numeric(X6),
   X7 = as.numeric(X7),
   X8 = as.numeric(X8),
   X9 = as.numeric(X9),          
   X10 = as.numeric(X10)      
 )
 str(df3)
             
df4 <- df3 %>% rename(country = `Carbon.Dioxide.emissions.from.fossil.fuel.consumption.and.cement.production.by.nation`,
  year = X2,
  `Total CO2 emissions from fossil-fuels and cement production (thousand metric tons of C)` = X3,
`Emissions from solid fuel consumption` = X4,
`Emissions from liquid fuel consumption` = X5,
`Emissions from gas fuel consumption` = X6,
`Emissions from cement production` = X7,
`Emissions from gas flaring` = X8,
`Per capita CO2 emissions (metric tons of carbon)` = X9,
`Emissions from bunker fuels` = X10
) 
  
  df5 <-df4 %>% group_by(country)
    
    
    
  g<-  df5 %>% ggplot() +
    geom_line(aes(year,`Total CO2 emissions from fossil-fuels and cement production (thousand metric tons of C)` ,color = country)) +
      theme(legend.position="none")
   
  
   ggplotly(g)
    
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
   total_co2 = X3 #`Total CO2 emissions from fossil-fuels and cement production (thousand metric tons of C)`
   ,
em_sf = X4, #`Emissions from solid fuel consumption` 
em_lf = X5, #`Emissions from liquid fuel consumption` 
em_gf = X6, #`Emissions from gas fuel consumption` 
em_cp = X7, #`Emissions from cement production` ,
em_gfl = X8, #`Emissions from gas flaring`
em_c02_pc = X9, #`Per capita CO2 emissions (metric tons of carbon)`
em_bf = X10 #`Emissions from bunker fuels`
) 
  
  df5 <-df4 %>% group_by(country)
    
    
    
  g<-  df5 %>% ggplot() +
    geom_line(aes(year,total_co2 ,color = country)) +
      theme(legend.position="none")
   
  
   ggplotly(g)
   
   
   
w_df <- df4 %>% group_by(year) %>% summarise(n=sum(total_co2))%>% ungroup()

  g<-  w_df %>% ggplot() +
    geom_point(aes(year,n)) +
     geom_line(aes(year,n)) +
     geom_smooth(aes(year,n,color= "red"))
  g
  
  
  n_df <- df4 %>% group_by(country) %>% summarise(total_co2=sum(total_co2))%>% ungroup() %>% filter(country= c)
c <- c("UNITED KINGDOM","SERBIA")
  n<-  n_df %>% ggplot() +
    geom_histogram(aes(total_co2)) +
     geom_line(aes(country,total_co2)) +
     geom_smooth(aes(country,n,color= "red"))
  n
  
  
  
  
  
  
    
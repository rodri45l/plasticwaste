library(tidyverse)
library(rio)
library(plotly)
library(janitor)
library(gganimate)

library(transformr)
#importamos los datos
df1<- read_csv("./Datos/nation_1751_2017.csv")
#Limpiamos el df y ponemos las variables como numericas
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
 #comprobamos que las variables del df3 estan correctas
 str(df3)
 #renombramos las variables para trabajar de manera mas cómoda
             
df4 <- df3 %>% rename(country = `Carbon.Dioxide.emissions.from.fossil.fuel.consumption.and.cement.production.by.nation`,
  year = X2,
   total_co2 = X3 #`Total CO2 emissions from fossil-fuels and cement production (thousand metric tons of C)`
   ,
em_sf = X4, #`Emissions from solid fuel consumption` 
em_lf = X5, #`Emissions from liquid fuel consumption` 
em_gf = X6, #`Emissions from gas fuel consumption` 
em_cp = X7, #`Emissions from cement production` ,
em_gfl = X8, #`Emissions from gas flaring`
em_co2_pc = X9, #`Per capita CO2 emissions (metric tons of carbon)`
em_bf = X10 #`Emissions from bunker fuels`
) 


#Gráfico evol. co2 por paises 
  df5 <-df4 %>% group_by(country)
    
    
    
  g<-  df5 %>% ggplot() +
    geom_line(aes(year,total_co2 ,color = country,group=country) ) +
      theme(legend.position="none")
  
  str(df5)
  g +  transition_reveal(year) +
    labs(title = "year: {as.integer(frame_along)}")
  
   ggplotly(g)
   
   #Gráfico acumulacion total co2 en el mundo
   
w_df <- df4 %>% group_by(year) %>% summarise(n=sum(total_co2))%>% ungroup()

  g<-  w_df %>% ggplot() +
    geom_point(aes(year,n)) +
     geom_line(aes(year,n)) g
  +geom_smooth(aes(year,n,color= "red"))
  g +  transition_reveal(year) +
    labs(title = "year: {as.integer(frame_along)}")
  #Grafico circular paises con mayor prod. co2 per capita
  
  n_df <- df4 %>% group_by(country) %>% summarise(total_co2=sum(total_co2))%>% ungroup() %>% slice_max(total_co2,n=20) %>% arrange(desc(total_co2))
  n_df2 <- df4 %>% filter(year > 2000) %>% group_by(country) %>% summarise(em_co2_pc=sum(em_co2_pc))%>% ungroup() %>% slice_max(em_co2_pc,n=15) %>% arrange(desc(em_co2_pc))
  k <- n_df2 %>% ggplot(aes(x = reorder(country, em_co2_pc),em_co2_pc,fill= country)) +
    geom_bar(stat = "identity") +
    theme(legend.position = "none") +
   
    coord_polar(start=0)
    
 k   
    
   
  n_df3 <- df4 %>% group_by(country) %>% summarise(em_co2_pc=sum(em_co2_pc))%>% ungroup()  %>% arrange(desc(em_co2_pc))
 
  
  
  #gráfico mayores productores co2 del mundo(acumulado total)
    
 p<- n_df %>% ggplot(aes(x = reorder(country, total_co2),total_co2,fill= country)) +
    geom_bar(stat = "identity") +
    theme(legend.position = "none") +
    coord_flip()
  p
  
  
  n_df6 <- df4 %>% group_by(country) %>% summarise(total_co2=sum(total_co2))%>% ungroup() %>% arrange(desc(total_co2))
    
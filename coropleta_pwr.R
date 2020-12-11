
library(tidyverse)
library(rio)
library(plotly)
library(janitor)
library(gganimate)
library(viridis)
library("sf")
library(tidyverse)

df_pwr<- read_csv("./datos/renpwr.csv") %>% select(-c(`Series Name`,`Series Code`))  %>% rename(sovereignt = `Country Name`,
          `1990` = `1990 [YR1990]`,
          `1991`=`1991 [YR1991]`,
          `1992`=`1992 [YR1992]`,
          `1993`=`1993 [YR1993]`,
          `1994`=`1994 [YR1994]`,
          `1995`=`1995 [YR1995]`,
          `1996`=`1996 [YR1996]`,
          `1997`=`1997 [YR1997]`,
          `1998`=`1998 [YR1998]`,
          `1999`=`1999 [YR1999]`,
          `2000`=`2000 [YR2000]`,
          `2001`=`2001 [YR2001]`,
          `2002`=`2002 [YR2002]`,
          `2003`=`2003 [YR2003]`,
          `2004`=`2004 [YR2004]`,
          `2005`=`2005 [YR2005]`,
          `2006`=`2006 [YR2006]`,
          `2007`=`2007 [YR2007]`,
          `2008`=`2008 [YR2008]`,
          `2009`=`2009 [YR2009]`,         
          `2010`=`2010 [YR2010]`,
          `2011`=`2011 [YR2011]`,
          `2012`=`2012 [YR2012]`,
          `2013`=`2013 [YR2013]`,
          `2014`=`2014 [YR2014]` ,  
          `2015`=`2015 [YR2015]`
         
  ) 
str(df_pwr)



df_pwr2 <- df_pwr %>%  pivot_longer(cols = 3:28, names_to = "year") %>% transform(year = as.numeric(year),
                                                                                  value = as.numeric(value)
                                                                                  ) %>% filter(year == 2015)
df_pwr3 <-df_pwr %>%  pivot_longer(cols = 3:28, names_to = "year") %>% transform(year = as.numeric(year),
                                                                                  value = as.numeric(value)
                                                                                  )






world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))





                              




df_pwr2$sovereignt <- recode(df_pwr2$sovereignt, 
                             "United States"="United States of America", 
                             "Russian Federation" = "Russia",
                             "Venezuela, RB" = "Venezuela",
                             "Congo, Dem. Rep." = "Democratic Republic of the Congo" ,
                             "	Congo, Rep."="Republic of Congo"	 ,
                             "Egypt, Arab Rep." = "Egypt" ,
                             "Tanzania" = "United Republic of Tanzania",
                             "Cote d'Ivoire" = "Ivory Coast",
                             "Yemen, Rep." = "Yemen",
                             "Iran, Islamic Rep." = "Iran",
                             "Syrian Arab Republic" = "Syria",
                             "Slovak Republic" = "Slovakia",
                             "Serbia" = "Republic of Serbia",
                             "North Macedonia" = "Macedonia",
                             "Kyrgyz Republic" = "Kyrgyzstan",
                             "Korea, Dem. People’s Rep" = "North Korea",
                             "Korea, Rep." = "South Korea",
                             "Lao PDR" = "Laos"
)


df_inner_pwr <- merge(world,df_pwr2,by="sovereignt",all=TRUE)
str(df_inner_pwr)


p <- ggplot(data = df_inner_pwr) + geom_sf() +
  labs(title = "Gráfico 1: Mapa del mundo energias renovables",
       caption = "Datos provenientes de World Bank")


p+ geom_sf(aes(fill = value, color =value)) + scale_fill_viridis(direction = 1, label = scales::number,begin = 0, end = 0.9) + 
  scale_color_viridis(direction = 1, label = scales::number,begin = 0, end = 0.9) 



#------------------------------------------
  
 



df_pwr$sovereignt <- recode(df_pwr$sovereignt, 
                             "United States"="United States of America", 
                             "Russian Federation" = "Russia",
                             "Venezuela, RB" = "Venezuela",
                             "Congo, Dem. Rep." = "Democratic Republic of the Congo" ,
                             "	Congo, Rep."="Republic of Congo"	 ,
                             "Egypt, Arab Rep." = "Egypt" ,
                             "Tanzania" = "United Republic of Tanzania",
                             "Cote d'Ivoire" = "Ivory Coast",
                             "Yemen, Rep." = "Yemen",
                             "Iran, Islamic Rep." = "Iran",
                             "Syrian Arab Republic" = "Syria",
                             "Slovak Republic" = "Slovakia",
                             "Serbia" = "Republic of Serbia",
                             "North Macedonia" = "Macedonia",
                             "Kyrgyz Republic" = "Kyrgyzstan",
                             "Korea, Dem. People’s Rep" = "North Korea",
                             "Korea, Rep." = "South Korea",
                             "Lao PDR" = "Laos")
df_pwr4 <- merge(world,df_pwr,by="sovereignt",all=TRUE)
df_pwr6 <- df_pwr4 %>%  pivot_longer(cols = 65:90, names_to = "year") %>% transform(year = as.numeric(year),
                                                                                  value = as.numeric(value)
                                                                                  ) %>% filter(year==2000)

str(df_pwr4)



o <- ggplot(data = df_pwr6) + geom_sf(data = df_pwr6, aes(fill = value)) +
  labs(title = "Gráfico 1: Mapa del mundo energias renovables",
       caption = "Datos provenientes de World Bank")


o + geom_sf(aes(fill = value, color =value)) + scale_fill_viridis(direction = 1, label = scales::number,begin = 0, end = 0.9) + 
  scale_color_viridis(direction = 1, label = scales::number,begin = 0, end = 0.9) 

 
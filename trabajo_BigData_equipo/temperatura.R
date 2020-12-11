library(tidyverse)
library(gganimate)
pct <- function(x) {((x/lag(x)
)-1)*2}
pct2 <- function(x) {((x/lag(x)
)-1)*100}


temp_country <-read_csv("./Datos/GlobalLandTemperaturesByCountry.csv")

temp_2000 <- subset(temp_country,dt> "1899-12-01")

#paises <- c("United States","China")
#temp_2000 <-temp_2000 %>% filter(Country == paises)
t_df <- temp_2000 %>% drop_na() %>%
mutate(month = format(dt, "%m"), year = format(dt, "%Y")) %>%
group_by(month, year) %>%
summarise(mean = mean(AverageTemperature))




t_df2 <- t_df %>% group_by(year) %>% summarise(tmean= mean(mean))
 t_df2 <-  transform(t_df2, year = as.numeric(year))

 w_df2 <- w_df %>% filter(year > 1899)
 w_df3 <- w_df2  %>%  mutate_each(funs(pct), n) %>% drop_na()
t_df3 <- t_df2  %>% mutate_each(funs(pct2), tmean) %>% drop_na()
w_df4 <- w_df3 %>% mutate(tco2 = cumsum(n))
t_df4 <- t_df3  %>% mutate(temp = cumsum(tmean))
 
 j <-ggplot() +
  
  
  geom_line(data =w_df3,aes(year,n),colour = "green") +
  #geom_smooth(data = w_df3,aes(year,n)) +
 theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
   #geom_smooth(data = t_df3,aes(year,tmean),colour= "purple") +
    geom_line(data = t_df3,aes(year,tmean,group=1),colour = "purple2") 
  
      
  
  
j + transition_reveal(year) +
  labs(title = "year: {as.integer(frame_along)}")



y <-ggplot() +
  
  
  geom_line(data =w_df4,aes(year,tco2),colour = "green") +
  #geom_smooth(data = w_df4,aes(year,tco2)) +
 #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
   #geom_smooth(data = t_df4,aes(year,temp),colour= "purple") +
    geom_line(data = t_df4,aes(year,temp,group=1),colour = "purple2") 
  
      
  
  
y+ transition_reveal(year) +
  labs(title = "year: {as.integer(frame_along)}")

l<- ggplot(wt_df,aes(n,tmean)) +
  geom_point() +
  geom_smooth() 

l + 
  stat_density_2d(aes(fill = ..level..), geom="polygon")+
  scale_fill_gradient(low="blue", high="red")


l +  geom_bin2d()


wt_df <- inner_join(w_df2,t_df2)
  
  
  
  
  
  
  
  
  cor(wt_df, method = "spearman", use = "complete.obs")
  
  








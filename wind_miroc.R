if (!require("tidyverse")) install.packages("tidyverse") ; library(tidyverse)
if (!require("readr")) install.packages("readr") ; library(readr)
if (!require("lubridate")) install.packages("lubridate") ; library(lubridate)
if (!require("reshape2")) install.packages("reshape2") ; library(reshape2)

require(ggplot2)
require(dplyr)

opt <- theme_bw()+
  theme(axis.title = element_text(face = "bold", color = "black", size = 20),
        axis.text.x = element_text(face = "plain", color = "black", 
                                   size = 18, angle = 90),
        axis.text.y = element_text(face = "plain", color = "black", size = 18),
        legend.text=element_text(size=20, face = "bold"),
        legend.title = element_text(size = 20, face = "bold"),
        strip.text.x = element_text(size = 18, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"),
        legend.position="none")

### local1 ----

wind1 <- read_delim("dados_miroc/Eta_MIROC5_Hist_1960_2005_W100_3h_P1.txt", 
                    ";", escape_double = FALSE, col_names = FALSE, 
                    trim_ws = TRUE)
wind1 <- wind1 %>% 
  mutate(date = ymd(substr(X1,1,8))) %>% 
  mutate(hour = substr(X1,9,10)) %>% 
  mutate(year = year(date)) %>% 
  na.omit() %>%  #o modelo retorna valores para dia 30 e 31 de fevereiro
  filter(year < 2006) %>% 
  mutate(group = c("1960 - 2006"))


wind2 <- read_delim("dados_miroc/Eta_MIROC5_RCP8.5_2006_2040_W100_3h_P1.txt", 
                    ";", escape_double = FALSE, col_names = FALSE, 
                    trim_ws = TRUE)
wind2 <- wind2 %>% 
  mutate(date = ymd(substr(X1,1,8))) %>% 
  mutate(hour = substr(X1,9,10)) %>% 
  mutate(year = year(date)) %>% 
  na.omit() %>%  #o modelo retorna valores para dia 30 e 31 de fevereiro
  filter(year < 2040) %>% 
  mutate(group = c("2006 - 2040"))


wind3 <- read_delim("dados_miroc/Eta_MIROC5_RCP8.5_2040_2070_W100_3h_P1.txt", 
                    ";", escape_double = FALSE, col_names = FALSE, 
                    trim_ws = TRUE)
wind3 <- wind3 %>% 
  mutate(date = ymd(substr(X1,1,8))) %>% 
  mutate(hour = substr(X1,9,10)) %>% 
  mutate(year = year(date)) %>% 
  na.omit() %>%  #o modelo retorna valores para dia 30 e 31 de fevereiro
  filter(year < 2070) %>% 
  mutate(group = c("2040 - 2070"))


wind4 <- read_delim("dados_miroc/Eta_MIROC5_RCP8.5_2070_2099_W100_3h_P1.txt", 
                    ";", escape_double = FALSE, col_names = FALSE, 
                    trim_ws = TRUE)
wind4 <- wind4 %>% 
  mutate(date = ymd(substr(X1,1,8))) %>% 
  mutate(hour = substr(X1,9,10)) %>% 
  mutate(year = year(date)) %>% 
  na.omit() %>%   #o modelo retorna valores para dia 30 e 31 de fevereiro
  mutate(group = c("2070 - 2100"))


wind <- rbind(wind1,wind2,wind3,wind4) %>% 
  mutate(group = cut(year, breaks = c(1960, 1990, 2021,2050,2070,2099),labels = c("1","2","3","4","5"))) %>% 
  filter(group != 2) %>% 
  filter(group != 4)


### Fiting the Weibull


require(broom)
require(purrr)
require(tidyr)
require(MASS)

# loading data


# Fitting the Weibull 

wind_weibull <- wind %>% 
  dplyr::select(X2, group) %>% 
  mutate(group = as.factor(group)) %>% 
  group_by(group) %>% 
  summarise(vec = X2 %>% list) %>% 
  mutate(mod = map(vec, ~MASS::fitdistr(.x, "Weibull"))) %>% 
  mutate(mod_est_shape = map(mod, ("estimate")) %>% map_dbl("shape")) %>% 
  mutate(mod_est_scale = map(mod, ("estimate")) %>% map_dbl("scale")) %>% 
  mutate(mod_glance = map(mod, glance)) %>% 
  dplyr::select(-vec, -mod) %>% 
  unnest()


### Weibulls equivalentes

a <- rweibull(wind_weibull$n[1], shape = wind_weibull$mod_est_shape[1], 
              scale = wind_weibull$mod_est_scale[1]) %>% 
  as.data.frame() %>% 
  mutate(group = c("1961 - 1990"))

b <- rweibull(wind_weibull$n[2], shape = wind_weibull$mod_est_shape[2], 
              scale = wind_weibull$mod_est_scale[2]) %>% 
  as.data.frame() %>% 
  mutate(group = c("2021 - 2050"))


c <- rweibull(wind_weibull$n[3], shape = wind_weibull$mod_est_shape[3], 
              scale = wind_weibull$mod_est_scale[1]) %>% 
  as.data.frame() %>% 
  mutate(group = c("2070 - 2099"))


weibull <- rbind(a,b,c) %>% 
  as.data.frame() %>% 
  mutate(points = weibull$.)


### local2 ----

wind1 <- read_delim("dados_miroc/Eta_MIROC5_Hist_1960_2005_W100_3h_P2.txt", 
                    ";", escape_double = FALSE, col_names = FALSE, 
                    trim_ws = TRUE)
wind1 <- wind1 %>% 
  mutate(date = ymd(substr(X1,1,8))) %>% 
  mutate(hour = substr(X1,9,10)) %>% 
  mutate(year = year(date)) %>% 
  na.omit() %>%  #o modelo retorna valores para dia 30 e 31 de fevereiro
  filter(year < 2006) %>% 
  mutate(group = c("1960 - 2006"))


wind2 <- read_delim("dados_miroc/Eta_MIROC5_RCP8.5_2006_2040_W100_3h_P2.txt", 
                    ";", escape_double = FALSE, col_names = FALSE, 
                    trim_ws = TRUE)
wind2 <- wind2 %>% 
  mutate(date = ymd(substr(X1,1,8))) %>% 
  mutate(hour = substr(X1,9,10)) %>% 
  mutate(year = year(date)) %>% 
  na.omit() %>%  #o modelo retorna valores para dia 30 e 31 de fevereiro
  filter(year < 2040) %>% 
  mutate(group = c("2006 - 2040"))


wind3 <- read_delim("dados_miroc/Eta_MIROC5_RCP8.5_2040_2070_W100_3h_P2.txt", 
                    ";", escape_double = FALSE, col_names = FALSE, 
                    trim_ws = TRUE)
wind3 <- wind3 %>% 
  mutate(date = ymd(substr(X1,1,8))) %>% 
  mutate(hour = substr(X1,9,10)) %>% 
  mutate(year = year(date)) %>% 
  na.omit() %>%  #o modelo retorna valores para dia 30 e 31 de fevereiro
  filter(year < 2070) %>% 
  mutate(group = c("2040 - 2070"))


wind4 <- read_delim("dados_miroc/Eta_MIROC5_RCP8.5_2070_2099_W100_3h_P2.txt", 
                    ";", escape_double = FALSE, col_names = FALSE, 
                    
                    trim_ws = TRUE)
wind4 <- wind4 %>% 
  mutate(date = ymd(substr(X1,1,8))) %>% 
  mutate(hour = substr(X1,9,10)) %>% 
  mutate(year = year(date)) %>% 
  na.omit() %>%   #o modelo retorna valores para dia 30 e 31 de fevereiro
  mutate(group = c("2070 - 2100"))


wind <- rbind(wind1,wind2,wind3,wind4) %>% 
  mutate(group = cut(year, breaks = c(1960, 1990, 2021,2050,2070,2099),labels = c("1","2","3","4","5"))) %>% 
  filter(group != 2) %>% 
  filter(group != 4)

### Fiting the Weibull


require(broom)
require(purrr)
require(tidyr)
require(MASS)



# Fitting the Weibull 

wind_weibull <- wind %>% 
  mutate(X2 = X2 + 0.001) %>% 
  dplyr::select(X2, group) %>% 
  mutate(group = as.factor(group)) %>% 
  group_by(group) %>% 
  summarise(vec = X2 %>% list) %>% 
  mutate(mod = map(vec, ~MASS::fitdistr(.x, "Weibull"))) %>% 
  mutate(mod_est_shape = map(mod, ("estimate")) %>% map_dbl("shape")) %>% 
  mutate(mod_est_scale = map(mod, ("estimate")) %>% map_dbl("scale")) %>% 
  mutate(mod_glance = map(mod, glance)) %>% 
  dplyr::select(-vec, -mod) %>% 
  unnest()


### Weibulls equivalentes

a <- rweibull(wind_weibull$n[1], shape = wind_weibull$mod_est_shape[1], 
              scale = wind_weibull$mod_est_scale[1]) %>% 
  as.data.frame()

b <- rweibull(wind_weibull$n[2], shape = wind_weibull$mod_est_shape[2], 
              scale = wind_weibull$mod_est_scale[2]) %>% 
  as.data.frame()


c <- rweibull(wind_weibull$n[3], shape = wind_weibull$mod_est_shape[3], 
              scale = wind_weibull$mod_est_scale[1]) %>% 
  as.data.frame() 


weibull <- rbind(a,b,c) %>% 
  as.data.frame() %>% 
  mutate(points = weibull$.)



### local3 ----

wind1 <- read_delim("dados_miroc/Eta_MIROC5_Hist_1960_2005_W100_3h_P3.txt", 
                    ";", escape_double = FALSE, col_names = FALSE, 
                    trim_ws = TRUE)

wind1 <- wind1 %>% 
  mutate(date = ymd(substr(X1,1,8))) %>% 
  mutate(hour = substr(X1,9,10)) %>% 
  mutate(year = year(date)) %>% 
  na.omit() %>%  #o modelo retorna valores para dia 30 e 31 de fevereiro
  filter(year < 2006) %>% 
  mutate(group = c("1960 - 2006"))


wind2 <- read_delim("dados_miroc/Eta_MIROC5_RCP8.5_2006_2040_W100_3h_P3.txt", 
                    ";", escape_double = FALSE, col_names = FALSE, 
                    trim_ws = TRUE)
wind2 <- wind2 %>% 
  mutate(date = ymd(substr(X1,1,8))) %>% 
  mutate(hour = substr(X1,9,10)) %>% 
  mutate(year = year(date)) %>% 
  na.omit() %>%  #o modelo retorna valores para dia 30 e 31 de fevereiro
  filter(year < 2040) %>% 
  mutate(group = c("2006 - 2040"))


wind3 <- read_delim("dados_miroc/Eta_MIROC5_RCP8.5_2040_2070_W100_3h_P3.txt", 
                    ";", escape_double = FALSE, col_names = FALSE, 
                    trim_ws = TRUE)
wind3 <- wind3 %>% 
  mutate(date = ymd(substr(X1,1,8))) %>% 
  mutate(hour = substr(X1,9,10)) %>% 
  mutate(year = year(date)) %>% 
  na.omit() %>%  #o modelo retorna valores para dia 30 e 31 de fevereiro
  filter(year < 2070) %>% 
  mutate(group = c("2040 - 2070"))


wind4 <- read_delim("dados_miroc/Eta_MIROC5_RCP8.5_2040_2070_W100_3h_P3.txt", 
                    ";", escape_double = FALSE, col_names = FALSE, 
                    
                    trim_ws = TRUE)
wind4 <- wind4 %>% 
  mutate(date = ymd(substr(X1,1,8))) %>% 
  mutate(hour = substr(X1,9,10)) %>% 
  mutate(year = year(date)) %>% 
  na.omit() %>%   #o modelo retorna valores para dia 30 e 31 de fevereiro
  mutate(group = c("2070 - 2100"))


wind <- rbind(wind1,wind2,wind3,wind4) %>% 
  mutate(group = cut(year, breaks = c(1960, 1990, 2021,2050,2070,2099),labels = c("1","2","3","4","5"))) %>% 
  filter(group != 3) %>% 
  filter(group != 4)

### Fiting the Weibull


require(broom)
require(purrr)
require(tidyr)
require(MASS)

# loading data


# Fitting the Weibull 

wind_weibull <- wind %>% 
  dplyr::select(X2, group) %>% 
  mutate(group = as.factor(group)) %>% 
  group_by(group) %>% 
  summarise(vec = X2 %>% list) %>% 
  mutate(mod = map(vec, ~MASS::fitdistr(.x, "Weibull"))) %>% 
  mutate(mod_est_shape = map(mod, ("estimate")) %>% map_dbl("shape")) %>% 
  mutate(mod_est_scale = map(mod, ("estimate")) %>% map_dbl("scale")) %>% 
  mutate(mod_glance = map(mod, glance)) %>% 
  dplyr::select(-vec, -mod) %>% 
  unnest()


### Weibulls equivalentes

a <- rweibull(wind_weibull$n[1], shape = wind_weibull$mod_est_shape[1], 
              scale = wind_weibull$mod_est_scale[1]) %>% 
  as.data.frame() %>% 
  mutate(group = c("1961 - 1990"))

b <- rweibull(wind_weibull$n[2], shape = wind_weibull$mod_est_shape[2], 
              scale = wind_weibull$mod_est_scale[2]) %>% 
  as.data.frame() %>% 
  mutate(group = c("2021 - 2050"))


c <- rweibull(wind_weibull$n[3], shape = wind_weibull$mod_est_shape[3], 
              scale = wind_weibull$mod_est_scale[1]) %>% 
  as.data.frame() %>% 
  mutate(group = c("2070 - 2099"))


weibull <- rbind(a,b,c) %>% 
  as.data.frame() %>% 
  mutate(points = weibull$.)


### local4 ----

wind1 <- read_delim("dados_miroc/Eta_MIROC5_Hist_1960_2005_W100_3h_P4.txt", 
                    ";", escape_double = FALSE, col_names = FALSE, 
                    trim_ws = TRUE)

wind1 <- wind1 %>% 
  mutate(date = ymd(substr(X1,1,8))) %>% 
  mutate(hour = substr(X1,9,10)) %>% 
  mutate(year = year(date)) %>% 
  na.omit() %>%  #o modelo retorna valores para dia 30 e 31 de fevereiro
  filter(year < 2006) %>% 
  mutate(group = c("1960 - 2006"))


wind2 <- read_delim("dados_miroc/Eta_MIROC5_RCP8.5_2006_2040_W100_3h_P4.txt", 
                    ";", escape_double = FALSE, col_names = FALSE, 
                    trim_ws = TRUE)
wind2 <- wind2 %>% 
  mutate(date = ymd(substr(X1,1,8))) %>% 
  mutate(hour = substr(X1,9,10)) %>% 
  mutate(year = year(date)) %>% 
  na.omit() %>%  #o modelo retorna valores para dia 30 e 31 de fevereiro
  filter(year < 2040) %>% 
  mutate(group = c("2006 - 2040"))


wind3 <- read_delim("dados_miroc/Eta_MIROC5_RCP8.5_2040_2070_W100_3h_P4.txt", 
                    ";", escape_double = FALSE, col_names = FALSE, 
                    trim_ws = TRUE)
wind3 <- wind3 %>% 
  mutate(date = ymd(substr(X1,1,8))) %>% 
  mutate(hour = substr(X1,9,10)) %>% 
  mutate(year = year(date)) %>% 
  na.omit() %>%  #o modelo retorna valores para dia 30 e 31 de fevereiro
  filter(year < 2070) %>% 
  mutate(group = c("2040 - 2070"))


wind4 <- read_delim("dados_miroc/Eta_MIROC5_RCP8.5_2070_2099_W100_3h_P4.txt", 
                    ";", escape_double = FALSE, col_names = FALSE, 
                    
                    trim_ws = TRUE)
wind4 <- wind4 %>% 
  mutate(date = ymd(substr(X1,1,8))) %>% 
  mutate(hour = substr(X1,9,10)) %>% 
  mutate(year = year(date)) %>% 
  na.omit() %>%   #o modelo retorna valores para dia 30 e 31 de fevereiro
  mutate(group = c("2070 - 2100"))


wind <- rbind(wind1,wind2,wind3,wind4) %>% 
  mutate(group = cut(year, breaks = c(1960, 1990, 2021,2050,2070,2099),labels = c("1","2","3","4","5"))) %>% 
  filter(group != 3) %>% 
  filter(group != 4)

### Fiting the Weibull


require(broom)
require(purrr)
require(tidyr)
require(MASS)



# Fitting the Weibull 

wind_weibull <- wind %>% 
  dplyr::select(X2, group) %>% 
  mutate(group = as.factor(group)) %>% 
  group_by(group) %>% 
  summarise(vec = X2 %>% list) %>% 
  mutate(mod = map(vec, ~MASS::fitdistr(.x, "Weibull"))) %>% 
  mutate(mod_est_shape = map(mod, ("estimate")) %>% map_dbl("shape")) %>% 
  mutate(mod_est_scale = map(mod, ("estimate")) %>% map_dbl("scale")) %>% 
  mutate(mod_glance = map(mod, glance)) %>% 
  dplyr::select(-vec, -mod) %>% 
  unnest()


### Weibulls equivalentes

a <- rweibull(wind_weibull$n[1], shape = wind_weibull$mod_est_shape[1], 
              scale = wind_weibull$mod_est_scale[1]) %>% 
  as.data.frame() %>% 
  mutate(group = c("1961 - 1990"))

b <- rweibull(wind_weibull$n[2], shape = wind_weibull$mod_est_shape[2], 
              scale = wind_weibull$mod_est_scale[2]) %>% 
  as.data.frame() %>% 
  mutate(group = c("2021 - 2050"))


c <- rweibull(wind_weibull$n[3], shape = wind_weibull$mod_est_shape[3], 
              scale = wind_weibull$mod_est_scale[1]) %>% 
  as.data.frame() %>% 
  mutate(group = c("2070 - 2099"))


weibull <- rbind(a,b,c) %>% 
  as.data.frame() %>% 
  mutate(points = weibull$.)

# Libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(readxl)
library(ggthemes)
library(tidyr)
library(scales)

# Read xl -----------------------------------------------------------------

custo <- read_excel("custo_vida.xlsx")
IGPM <- read_excel("IGPM.xls")

# Create variables --------------------------------------------------------

custo %>%
  mutate(date = as.Date(paste(1,mes,ano, sep="/"), format = "%d/%m/%Y")) %>%
  gather(variable, value, luz:onibus) %>%
  left_join(IGPM, by=c("mes"="MES", "ano"="ANO")) %>% 
  group_by(variable) %>% 
  arrange(date) %>% 
  mutate(lag = lag(value), 
         change = (value / lag) - 1,
         change = ifelse(is.na(change), 0, change),
         cumsum = cumsum(change),
         rlag = lag(value/FIM/100),
         rchange = (value/FIM/100 / rlag) - 1,
         rchange = ifelse(is.na(rchange), 0, rchange),
         rcumsum = cumsum(rchange)) -> custo2


# Plots -------------------------------------------------------------------

custo2 %>% 
  ggplot(aes(x=date, y=change, col=variable)) +
  geom_line() +
  theme_fivethirtyeight()



## Nominal
custo2 %>% 
  ggplot(aes(x=date, y=cumsum, col=variable)) +
  geom_line() +
  theme_fivethirtyeight() +
  labs(title="Variação nominal de 6 itens na cidade de SP",
       caption="Fonte: DIEESE",
       x="Data", y="Percentual") +
  scale_y_continuous(label = percent) +
  scale_color_brewer(type="qual", palette = 6)


## Real
custo2 %>%
  ggplot(aes(x=date, y=rcumsum, col=variable)) +
  geom_line() +
  theme_fivethirtyeight() +
  labs(title="Variaçao real de 6 itens na cidade de SP",
       caption="Fonte: DIEESE",
       x="Data", y="Percentual") +
  scale_y_continuous(label = percent) +
  scale_color_brewer(type="qual", palette = 6)











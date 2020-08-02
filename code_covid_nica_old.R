#transpose
nica_covid_confirmados <- reshape2::melt(covid_nica_confirmados)

#Add 0s for dep that starts with >0
Rivas.Covid <- data.frame(as.factor("Rivas"), as.factor("X17.3.2020"), as.integer(0))
Matagalpa.Covid <- data.frame(as.factor("Matagalpa"), as.factor("X17.3.2020"), as.integer(0))
Total.Covid <- data.frame(as.factor("Total"), as.factor("X17.3.2020"), as.integer(0))
No_información.Covid <- data.frame(as.factor("No información"), as.factor("X17.3.2020"), as.integer(0))
names(Rivas.Covid) <- c("dep", "variable", "value")  
names(Matagalpa.Covid) <- c("dep", "variable", "value")  
names(Total.Covid) <- c("dep", "variable", "value")  
names(No_información.Covid) <- c("dep", "variable", "value")  

nica_covid_confirmados <- rbind(nica_covid_confirmados, Rivas.Covid, Matagalpa.Covid, Total.Covid, No_información.Covid) 

# create days from moment >0
nica_covid_confirmados <- nica_covid_confirmados %>% 
  rename(fecha = 'variable', departamento = 'dep', casos = 'value') %>%
  mutate(fecha = gsub("[a-zA-Z]", "", fecha),
         fecha = as.character(as.Date(fecha,"%d.%m.%Y")),
         departamento = as.character(departamento),
         casos = as.numeric(casos)) %>%
  group_by(departamento) %>%
  mutate(fecha = sort(fecha), 
         casos = sort(casos),
         dias = round(as.numeric(difftime(fecha, na.locf(lag(ifelse(casos == 0, fecha, NA)),na.rm=FALSE))))) %>%
  filter(casos!= 0) 

# graph curves (todas)
nica_covid_confirmados %>% 
  filter(!(departamento %in% c("Total"))) %>%
  ggplot(aes(as.numeric(dias), as.numeric(casos), col=departamento)) +
  geom_point(show.legend=TRUE) +
  geom_line() +
  scale_y_continuous(limit=c(0,3200)) +
  ggtitle("Casos acumulados en Nicaragua") +
  ylab("no. de casos sospechosos") +
  xlab("no. de días desde 1er caso")

# graph curves (por municipio)
nica_covid_confirmados %>% 
  filter(!(departamento %in% c("Total", "Managua"))) %>%
  ggplot(aes(as.numeric(dias), as.numeric(casos), col=departamento)) +
  geom_point(show.legend=TRUE) +
  geom_line() +
  scale_y_continuous(limit=c(0,800)) +
  ylab("no. de casos sospechosos") +
  xlab("no. de días desde 1er caso") +
  facet_wrap(.~departamento) +
  labs(title = "Gráfico 2: Casos acumulados por municipios", 
       caption = "Fuente: Observatorio Ciudadano COVID-19 Nicaragua") +
  theme(legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1, vjust = 0.9, size = 8)) 

# extract last observation
nica_covid_confirmados %>%
  group_by(departamento) %>% 
  do(tail(., 1)) %>%
  write.csv(., "/Users/quinrod/projects/R/COVID-19/covid_nica_confirmados.csv", row.names = FALSE)
  
# extract original table
covid_nica_confirmados %>%
  write.csv(.,"/Users/quinrod/projects/R/COVID-19/covid_nica_confirmados.csv", row.names = FALSE)

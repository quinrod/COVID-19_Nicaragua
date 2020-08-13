library(data.table)
library(zoo)
library(RCurl)
library(tidyverse)
library(reshape2)
library(magrittr)
library(ggplot2)
library(plotly)
library(gridExtra)
library(ggpubr)

rdata<- '/Users/quinrod/projects/GitHub/COVID-19_Nicaragua/rdata/Nicaragua/'

# 1. extract data
covid_nica_confirmados <- getURL ("https://raw.githubusercontent.com/alvarole/shinden/master/covid19/data/observatorioNic_confirmados.csv")
covid_nica_confirmados <- read.csv(text = covid_nica_confirmados) %>%
write.csv(.,paste(rdata,'covid_nica_confirmados.csv'), row.names = FALSE)

covid_nica_fallecidos <- getURL("https://raw.githubusercontent.com/alvarole/shinden/master/covid19/data/observatorioNic_fallecidos.csv")
covid_nica_fallecidos <- read.csv(text = covid_nica_fallecidos) %>%
  write.csv(.,paste(rdata,'covid_nica_fallecidos.csv'), row.names = FALSE)

######## Point to your data directory 
series_all_files <- list.files(rdata)
series_data_files <- series_all_files[grepl('.csv', series_all_files)]

print(sprintf('Total data files = %s', length(series_data_files)))

series_data_ <- lapply(series_data_files, 
                       function(i) {
                         dat   <- read.csv(paste0(rdata, '/', i), stringsAsFactors = FALSE)
                         file_ <- gsub('.csv', '', i)
                         dat$Status <- strsplit(file_, '_')[[1]][3]
                         dat
                       })

series_data_[[1]] <- cbind(series_data_[[1]][1],series_data_[[1]][ncol(series_data_[[1]])],X17.3.2020 = 0,series_data_[[1]][2:ncol(series_data_[[1]])])
series_data_[[1]] <- series_data_[[1]][,-ncol(series_data_[[1]])]
series_data_[[2]] <- cbind(series_data_[[2]][1],series_data_[[2]][ncol(series_data_[[2]])],X17.3.2020 = 0, X18.3.2020 = 0, X19.3.2020 = 0, X20.3.2020 = 0, X21.3.2020 = 0,series_data_[[2]][2:ncol(series_data_[[2]])]) 
series_data_[[2]] <- series_data_[[2]][,-ncol(series_data_[[2]])]

### 2. Sanity check
# check whether thecolumn names of 3 datasets match up 
columns <- sapply(series_data_, colnames)

### The code below certainly works for datasets with small numbers of columns, 
### However, what if we have 1000 columns to do pair-wise checking, 
### or additional columns being added to the datasource? 
all(columns[, 1] == columns[, 2])

###========== the DRY principle: DON'T REPEAT YOURSELF
### let's do it in a smarter way :)
comb_cols <- combn(seq(ncol(columns)), 2)

check_colnames <- function(x, y){
  identical(columns[, x], columns[, y])
}

colnames_match <- sapply(seq(ncol(comb_cols)), function(i) check_colnames(x = comb_cols[1, i], y = comb_cols[2, i]))
stopifnot(all(colnames_match))

#### 3. Append a list of datasets into one single data frame 
series_df <- do.call(rbind, series_data_)

#### 4. Create status column

# Process data and subset on countries
# sort(unique(series_df$dep))

## sum group by country by status (all columns)
date_col_idx <- which(grepl('X', colnames(series_df)))

dep_data <- series_df %>%
  select(c(dep, Status, colnames(.)[date_col_idx])) %>%
  group_by(dep, Status) %>%
  summarise_each(list(sum)) %>%
  data.frame()

### Change the colnames for the dates
colnames(dep_data)[3:ncol(dep_data)] = gsub('X', '', colnames(dep_data)[3:ncol(dep_data)]) %>%
  gsub('\\.', '_', .)

#### 5. Generate and save graphs

figures<- '/Users/quinrod/projects/GitHub/COVID-19_Nicaragua/figures/'

# a. Heatmap
library(gplots)
my_palette <- colorRampPalette(c("light blue", "black", "dark red"))(n = 1000)
selected_dep <- c('No información', 'Total',
                  'Carazo', 'RACCN', 'RACCS', 
                  'Río San Juan ', 'Nueva Segovia', 
                  'Boaco', 'Madriz', 'Rivas',
                  'Jinotega', 'Chontales', 'Granada')

heatmap_dat <- dep_data %>%
  filter(Status == 'fallecidos',
         (!(dep %in% selected_dep)))

## subset: after Feb 22nd
col_idx <- which(colnames(heatmap_dat) == '1_5_2020')
heatmap_dat <- heatmap_dat[, c(1, 2, col_idx:ncol(heatmap_dat))]
rownames(heatmap_dat) <- unique(heatmap_dat$dep)

#### Change the Date format to M/DD:
heatmap_dat <- heatmap_dat %>%
  select(-dep, -Status) %>%
  set_colnames(paste0(sapply(strsplit(colnames(.), '_'), '[[', 1),
                      '/',
                      sapply(strsplit(colnames(.), '_'), '[[', 2)))

x <- as.matrix(heatmap_dat) 
x <- x[order(x[,ncol(x)],decreasing=T),]

png(paste(figures,"heatmap.png"), width = 16, height = 12, units = 'in', res = 800)
heatmap <- heatmap.2(x, trace = 'none', dendrogram = 'none',
                     density.info = 'none', keysize = 0.8,
                     key.title = NA, cexCol = 1, cexRow = 1,
                     col = my_palette, Colv = FALSE, Rowv = FALSE,
                     srtCol = 90, main = 'Casos de fallecidos por COVID-19', 
                     margins = c(8,8), 
                     xlab = 'Fuente: Observatorio Ciudadano COVID-19 Nicaragua')

dev.off()

# b. Static & Dynamic Longitudinal Spaghetti Chart

#### Select the countries for plotting, and convert wide format to long
current_dat <- dep_data %>%
  reshape2::melt(.) %>%
  set_colnames(c('State', 'Status', 'Date', 'Total')) #by reshaping it adds to total

## starts with the date when 1st case confirmed
find_case1_onwards <- function(dep_name) {
  case1_idx = which(current_dat[current_dat$State == dep_name, 'Total'] > 0)[1] #code for first date total was above 1
  
  this_dep = current_dat %>%
    filter(State == dep_name) %>%
    .[case1_idx:nrow(.), ]
}

current_dat_list <- lapply(unique(current_dat$State), find_case1_onwards)
current_dat <- do.call(rbind, current_dat_list)

# b.1. Dynamic Longitudinal Spaghetti Chart
#### Plotly with ggplot facet_wrap
docs<- '/Users/quinrod/projects/GitHub/COVID-19_Nicaragua/docs/'
selected_dep <- c('Chinandega', 'Managua', 'Masaya', 'Matagalpa', 'Jinotega','Granada',
                  'Total', 'Carazo', 'RACCN', 'RACCS','Río San Juan ', 'Nueva Segovia', 
                  'Boaco', 'Madriz', 'Rivas','Chontales', 'Estelí', 'León')

x <- lapply(selected_dep, 
                function(i) {
                    d <- current_dat %>% filter(State == i) 
  
                    state_plot <- d %>%
                        ggplot(aes_string(x = 'Date', y = 'Total', color = 'Status', group = 'Status', linetype = 'Status')) +
                         geom_line(lwd = 1.2) +
                          labs(title = paste('Trayectoria COVID-19 en', i)) +
                           xlab('Fuente: Observatorio Ciudadano COVID-19 Nicaragua') + 
                           ylab('Total Casos') +
                      
                           theme_bw() +
                           theme(plot.title = element_text(hjust = 0.5, vjust = 0.5),
                                 plot.subtitle = element_text(hjust = 3),
                                 plot.caption = element_text(hjust = 3),
                                 axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 5), 
                                 panel.spacing.y = unit(5, "mm"),
                                 panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank())
  
                          build <- plotly_build(state_plot) 
                          build$layout$width = 3200
                          build$layout$height = 900
                          build 
                          setwd(docs)
                          htmlwidgets::saveWidget(as_widget(build), paste(i,"trayectoria.html"))
                          })

x
setwd("/Users/quinrod")

#facet_wrap(~ State, scales = "free") +

# b.2. Static
filter <- c('confirmados')

static <- current_dat %>% 
  mutate(Date = as.character(as.Date(Date,"%d_%m_%Y")),
         State = as.character(State),
         Total = as.numeric(Total)) %>%
  filter(Status == filter) %>%
  group_by(State, Status) %>%
  mutate(Date = sort(Date), 
         Total = sort(Total),
         Days = 1:n())

#all in one
dev.off()
excluded_states <- c('Managua','Total', 'No información')

static <- static %>% 
  filter(!(State %in% excluded_states)) %>%
  ggplot(aes(as.numeric(Days), as.numeric(Total), col=State)) +
  geom_point(show.legend=TRUE) +
  geom_line() +
  scale_y_continuous(limit=c(0,1200)) +
  ylab("no. de casos confirmados") +
  xlab("no. de días desde 1er caso") +
  labs(title = "Casos acumulados por municipios", 
       caption = "Fuente: Observatorio Ciudadano COVID-19 Nicaragua") +
  theme(legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1, vjust = 0.9, size = 8)) 

ggsave(paste(figures,'todos en una.png'), 
       device = "png", 
       width = 16,
       height = 12,
       units = 'in')

dev.off()

#por municipio
static %>% 
  ggplot(aes(as.numeric(Days), as.numeric(Total), col=State)) +
  geom_point(show.legend=TRUE) +
  geom_line() +
  scale_y_continuous(limit=c(0,800)) +
  ylab("no. de casos confirmados") +
  xlab("no. de días desde 1er caso") +
  facet_wrap(.~State) +
  labs(title = "Casos acumulados por municipios", 
       caption = "Fuente: Observatorio Ciudadano COVID-19 Nicaragua") +
  theme(legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1, vjust = 0.9, size = 8)) 


# c. Animated Bar Charts in R

######==== 3) Animated racing bar chart
library(gganimate)
library(ggrepel)

selected_dep <- c('No información', 'Total')

##### INCLUDE ALL COUNTRIES and filter by top 10 rank 
dep_data = series_df %>% 
  filter(!(dep %in% selected_dep)) %>% 
  select(c(dep, Status, colnames(.)[date_col_idx])) %>%
  group_by(dep, Status) %>%
  summarise_each(funs(sum)) %>% 
  data.frame() 

colnames(dep_data)[3:ncol(dep_data)] = gsub('X', '', colnames(dep_data)[3:ncol(dep_data)]) %>%
  gsub('\\.', '_', .)

### process Date shown in the annotation: 
format_date = function(dat, date_var){
  dmy = strsplit(as.character(dat[[date_var]]), '_')
  day_n = sapply(dmy, '[[', 1)
  mon_n = sapply(dmy, '[[', 2)
  mon_c = ifelse(mon_n == '1', 'Jan', 
                 ifelse(mon_n == '2', 'Febrero',
                 ifelse(mon_n == '3', 'Marzo',
                 ifelse(mon_n == '4', 'Abril',
                 ifelse(mon_n == '5', 'Mayo',
                 ifelse(mon_n == '6', 'Junio',   
                 ifelse(mon_n == '7', 'Julio',
                 ifelse(mon_n == '8', 'Agosto',
                 ifelse(mon_n == '9', 'Septiembre',
                 ifelse(mon_n == '10', 'Octubre',
                 ifelse(mon_n == '11', 'Noviembre',       
                 ifelse(mon_n == '12', 'Diciembre',0))))))))))))
  return(sprintf('%s %s, 2020', day_n, mon_c))
}


## Create loop for two datasets
status <- c('confirmados','fallecidos')
cases <- lapply(status, 
            function(i) {
              casos <- dep_data[dep_data$Status == i, ] %>% 
                melt(.) %>% 
                set_colnames(c('State', 'Status', 'Date', 'Total')) %>%
                mutate(Date = format_date(dat = ., date_var = 'Date'),
                       Date = factor(Date, levels = unique(Date)))
              
              ##  levels(casos$Date)  
              ##  stopifnot(all(levels(casos_formatted$Date) == levels(casos$Date)))
              
              ##### set value_rel = Total/Total[floor(rank)==1] to avoid same multiple value as max, and there's no rank == 1
              casos_formatted <- casos %>%
                group_by(Date) %>%
                mutate(Total_all = sum(Total),
                       fixed_y = max(Total),
                       rank = rank(-Total),
                       value_lbl = paste0(" ", formatC(Total, big.mark = ','))) %>%
                group_by(State) %>%
                filter(rank <= 10) 
              
              #####======= static and animated plots
              total_text_y = 0.70*(max(casos_formatted$Total))
              panel_size_y = max(casos_formatted$Total) * 1  
              vline_original_y = seq(floor(max(casos_formatted$Total)/8), 
                                     max(casos_formatted$Total), by = floor(max(casos_formatted$Total)/8))
              
              country_font_size = 10
              bar_end_num_size = 10
              
              staticplot <- ggplot(casos_formatted,
                                   aes(rank, group = casos_formatted$State,
                                       fill = as.factor(casos_formatted$State), color = as.factor(casos_formatted$State))) +
                geom_tile(aes(y = Total/2, height = Total, width = 0.9), 
                          alpha = 0.9, color = NA) +
                geom_text(aes(y = 0, label = casos_formatted$State), vjust = 1.5, hjust = 0, 
                          size = country_font_size, fontface = "bold", color = 'white') +
                geom_text(aes(y = Total, label = value_lbl, vjust = 0.001, hjust = 0), fontface = 'bold', size = bar_end_num_size) +
                
                geom_text(aes(x = 8, y = total_text_y,
                              label = sprintf('%s\n Total Nacional =%s', Date, format(Total_all, big.mark=",", scientific=FALSE))),
                          size = 13, color = 'grey') +
                
                geom_hline(yintercept = vline_original_y, size = .08, color = "grey", linetype = 'dotted') +
                scale_y_continuous(labels = scales::comma) +
                scale_x_reverse() +
                coord_flip(clip = "off", expand = FALSE) +
                guides(color = FALSE, fill = FALSE) +
                theme(axis.line = element_blank(),
                      axis.text.y = element_blank(),
                      axis.ticks = element_blank(),
                      axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      legend.position = "none",
                      
                      plot.background = element_rect(fill = "black"),
                      panel.background = element_rect(fill = 'black'),
                      
                      panel.border=element_blank(),
                      
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      
                      plot.title = element_text(size=40, face="bold", colour='grey', vjust=1),  
                      plot.subtitle = element_text(size=18, face="italic", color="grey"),   
                      plot.caption = element_text(size=15, hjust=0.5, face="italic", color="grey"))
              
              #plot.margin = margin(2, 5, 2, 8, "cm"))
              
              #### Specify the transition length and ease_aes to give it a smoother transition 
              current_state_len = 0 
              current_transition_len = 12  
              
              anim <- staticplot + 
                transition_states(Date, transition_length = current_transition_len, state_length = current_state_len) + 
                ease_aes('cubic-in-out') +   
                view_follow(fixed_x = TRUE, fixed_y = c(-10, NA))  + 
                labs(title = paste('Casos' ,i, 'por día: {closest_state}'),
                     subtitle = 'Top 10 Departamentos',
                     caption = sprintf("Fuente: Observatorio Ciudadano COVID-19 Nicaragua\n Datos Extraídos at %s 23:59 (UTC)", 
                                       as.character(casos_formatted$Date[dim(casos_formatted)[1]])))
              
              #####===== Save the output file 
              library(gifski)
              
              output_type = 'GIF'
              animate_speed = 16 
              setwd(figures)
              if(output_type == 'GIF'){  ### Save as GIF
                save_name = paste(i,".gif")
                animate(anim, 500, fps = animate_speed, 
                        width = 1500, height = 1000, end_pause = 10, start_pause = 10, 
                        renderer = gifski_renderer(save_name))  
                
                print(sprintf('==== GIF file %s saved ====', save_name))
                
              } else {              ### Save as MP4
                save_name = paste(i,".mp4")
                
                animate(anim, 500, fps = animate_speed, 
                        width = 1500, height = 1000, end_pause = 30, start_pause = 20,
                        renderer = av_renderer(), 
                        rewind = FALSE) -> save_as_mp4
                
                anim_save(save_name, animation = save_as_mp4)
                
                print(sprintf('==== MP4 file %s saved ====', save_name))
              }  
              
            })
              
setwd("/Users/quinrod")




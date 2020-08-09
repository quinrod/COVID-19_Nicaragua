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

rdata<- '/Users/quinrod/projects/GitHub/COVID-19_Nicaragua/rdata/Global/'

# 1. extract data in R
covid_global_confirmados <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
covid_global_confirmados <- read.csv(text = covid_global_confirmados) %>%
   write.csv(.,paste(rdata,'covid_global_confirmados.csv'), row.names = FALSE)

covid_global_fallecidos <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
covid_global_fallecidos <- read.csv(text = covid_global_fallecidos) %>%
  write.csv(.,paste(rdata,'covid_global_fallecidos.csv'), row.names = FALSE)

covid_global_recuperados <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
covid_global_recuperados <- read.csv(text = covid_global_recuperados) %>%
  write.csv(.,paste(rdata,'covid_global_recuperados.csv'), row.names = FALSE)

######## Point to your data directory 
series_all_files <- list.files(rdata)
series_data_files <- series_all_files[grepl('.csv', series_all_files)]

print(sprintf('Total data files = %s', length(series_data_files)))

series_data_ <- lapply(series_data_files, 
                      function(i) {
                        dat <- read.csv(paste0(rdata, '/', i), stringsAsFactors = FALSE)
                        file_ <- gsub('.csv', '', i)
                        dat$Status <- strsplit(file_, '_')[[1]][3]
                        dat
                      })

### 2. Sanity check
# check whether thecolumn names of 3 datasets match up 
columns <- sapply(series_data_, colnames)

### The code below certainly works for datasets with small numbers of columns, 
### However, what if we have 1000 columns to do pair-wise checking, 
### or additional columns being added to the datasource? 
all(columns[, 1] == columns[, 2])
all(columns[, 2] == columns[, 3])
all(columns[, 1] == columns[, 3])

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

##### (optional) Recode the values 
series_df[which(series_df$Country.Region == 'United Kingdom'), 'Country.Region'] = 'UK'
series_df[which(series_df$Country.Region == 'Korea, South'), 'Country.Region'] = 'Korea S'

#### 4. Create status column

# Process data and subset on countries
# sort(unique(series_df$Country.Region))
selected_countries <- c('China', 'Italy', 'US', 'Iran')

## sum group by country by status (all columns)
date_col_idx <- which(grepl('X', colnames(series_df)))

country_data <- series_df %>%
  filter(Country.Region %in% selected_countries) %>%
  select(c(Country.Region, Status, colnames(.)[date_col_idx])) %>%
  group_by(Country.Region, Status) %>%
  summarise_each(list(sum)) %>%
  data.frame()

### Change the colnames for the dates
colnames(country_data)[3:ncol(country_data)] = gsub('X', '', colnames(country_data)[3:ncol(country_data)]) %>%
  gsub('\\.', '_', .)

#### 5. Generate graphs

# a. Heatmap
library(gplots)
my_palette <- colorRampPalette(c("light blue", "black", "red"))(n = 1000)

heatmap_dat <- country_data %>%
  filter(Status == 'confirmados')

## subset: after Feb 22nd
col_idx <- which(colnames(heatmap_dat) == '2_22_20')
heatmap_dat <- heatmap_dat[, c(1, 2, col_idx:ncol(heatmap_dat))]
rownames(heatmap_dat) <- unique(heatmap_dat$Country.Region)

#### Change the Date format to M/DD:
heatmap_dat <- heatmap_dat %>%
  select(-Country.Region, -Status) %>%
  set_colnames(paste0(sapply(strsplit(colnames(.), '_'), '[[', 1),
                      '/',
                      sapply(strsplit(colnames(.), '_'), '[[', 2)))

x <- as.matrix(heatmap_dat) 

dev.off()

heatmap <- heatmap.2(x, trace = 'none', dendrogram = 'none',
                            density.info = 'none', keysize = 0.8,
                            key.title = NA, cexRow = 1, cexCol = 0.7,
                            col = my_palette, Colv = FALSE,
                            srtCol = 90, main = 'Confirmed Cases for Selected Countries') 
  
# b. Interactive Longitudinal Spaghetti Chart

#### Select the countries for plotting, and convert wide format to long
current_dat <- country_data %>%
  filter(Country.Region %in% selected_countries) %>%
  reshape2::melt(.) %>%
  set_colnames(c('Country', 'Status', 'Date', 'Total')) #by reshaping it adds to total

## starts with the date when 1st case confirmed
find_case1_onwards <- function(country_name) {
  case1_idx = which(current_dat[current_dat$Country == country_name, 'Total'] > 0)[1] #code for first date total was above 1
  
  this_country = current_dat %>%
    filter(Country == country_name) %>%
    .[case1_idx:nrow(.), ]
}

current_dat_list <- lapply(unique(current_dat$Country), find_case1_onwards)
current_dat <- do.call(rbind, current_dat_list)

#### Plotly with ggplot facet_wrap
country_plot = ggplot(dat = current_dat,
                      aes_string(x = 'Date', y = 'Total', color = 'Status', group = 'Status', linetype = 'Status')) +
  geom_line(lwd = 1.2) +
  
  facet_wrap(~ Country, scales = "free") +
  
  labs(title = sprintf('Trajectories of the Status of Coronavirus \n')) +
  xlab('Date') + ylab('Total Numbers') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.spacing.y = unit(8, "mm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

build <- plotly_build(country_plot)
build$layout$width = 1000
build$layout$height = 900
build

# c. Animated Bar Charts in R

######==== 3) Animated racing bar chart
library(gganimate)
library(ggrepel)

##### INCLUDE ALL COUNTRIES and filter by top 10 rank 
country_data = series_df %>% 
  filter(Country.Region != 'China') %>% 
  select(c(Country.Region, Status, colnames(.)[date_col_idx])) %>%
  group_by(Country.Region, Status) %>%
  summarise_each(funs(sum)) %>% 
  data.frame() 

colnames(country_data)[3:ncol(country_data)] = gsub('X', '', colnames(country_data)[3:ncol(country_data)]) %>%
  gsub('\\.', '_', .)

confirmados = country_data[country_data$Status == 'confirmados', ] %>% 
  melt(.) %>% 
  set_colnames(c('Country.Region', 'Status', 'Date', 'Total')) 


### process Date shown in the annotation: 
format_date = function(dat, date_var){
  mdy = strsplit(as.character(dat[[date_var]]), '_')
  mon_n = sapply(mdy, '[[', 1)
  day_n = sapply(mdy, '[[', 2)
  
  mon_c = ifelse(mon_n == '1', 'Jan', ifelse(mon_n == '2', 'Feb', 'Mar'))
  return(sprintf('%s %s,2020', mon_c, day_n))
}

confirmados$Date = format_date(dat = confirmados, date_var = 'Date')
confirmados$Date = factor(confirmados$Date, levels = unique(confirmados$Date))

## levels(confirmados$Date)

##### set value_rel = Total/Total[floor(rank)==1] to avoid same multiple value as max, and there's no rank == 1
confirmados_formatted = confirmados %>% 
  group_by(Date) %>% 
  mutate(Total_all = sum(Total), 
         fixed_y = max(Total), 
         rank = rank(-Total), 
         value_rel = Total/Total[floor(rank)==1]+0.02,
         Value_lbl = paste0(" ", formatC(Total, big.mark = ','))) %>%
  group_by(Country.Region) %>%
  filter(rank <= 10) 

stopifnot(all(levels(confirmados_formatted$Date) == levels(confirmados$Date)))

#####======= static and animated plots
total_text_y = 0.87*(max(confirmados_formatted$Total))
panel_size_y = max(confirmados_formatted$Total) * 1.15  
vline_original_y = seq(floor(max(confirmados_formatted$Total)/8), 
                       max(confirmados_formatted$Total), by = floor(max(confirmados_formatted$Total)/8))

country_font_size = 8
bar_end_num_size = 10

staticplot = ggplot(confirmados_formatted, 
                    aes(rank, group = Country.Region,
                        fill = as.factor(Country.Region), color = as.factor(Country.Region))) +
  geom_tile(aes(y = Total/2, height = Total, width = 0.9), 
            alpha = 0.9, color = NA) +
  geom_text(aes(y = 0, label = paste(Country.Region, " ")), vjust = 0.2, hjust = 1, 
            size = country_font_size, fontface = "bold") +
  geom_text(aes(y = Total, label = Value_lbl, hjust = 0), fontface = 'bold', size = bar_end_num_size) +
  
  geom_text(aes(x = 8, y = total_text_y,
                label = sprintf('%s\n Global Total =%s', Date, format(Total_all, big.mark=",", scientific=FALSE))),
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
current_transition_len = 3  

anim = staticplot + 
  transition_states(Date, transition_length = current_transition_len, state_length = current_state_len) + 
  ease_aes('cubic-in-out') +   
  view_follow(fixed_x = TRUE, fixed_y = c(-10, NA))  + 
  labs(title = 'Spead of Confirmed Cases per day: {closest_state}',
       subtitle = 'Top 10 Countries/Regions',
       caption = sprintf("Data Source: John Hopkins Unversity CSSE Data Repo\n Data Extracted at %s 23:59 (UTC)", 
                         as.character(confirmados_formatted$Date[dim(confirmados_formatted)[1]])))

#####===== Save the output file 
library(gifski)

output_type = 'GIF'
animate_speed = 16 

if(output_type == 'GIF'){  ### Save as GIF
  save_name = "/Users/quinrod/projects/R/COVID-19/covid19.gif"
  animate(anim, 500, fps = animate_speed, 
          width = 1500, height = 1000, end_pause = 10, start_pause = 10, 
          renderer = gifski_renderer(save_name))  
  
  print(sprintf('==== GIF file %s saved ====', save_name))
  
} else {              ### Save as MP4
  save_name = "/Users/quinrod/projects/R/COVID-19/covid19.mp4"
  
  animate(anim, 500, fps = animate_speed, 
          width = 1500, height = 1000, end_pause = 30, start_pause = 20,
          renderer = av_renderer(), 
          rewind = FALSE) -> save_as_mp4
  
  anim_save(save_name, animation = save_as_mp4)
  
  print(sprintf('==== MP4 file %s saved ====', save_name))
}

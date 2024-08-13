## This script prepares pH sensor datasets from SAMI, Idronaut, EXO2, TROLL, and
## ProDSS sensors.
## Unsure about Seabird.
##
## Quinn Mackay and Peter Regier and Carolina Torres Sanchez
## 2024-07-12 (Updated 7/23)
##
# ########## #
# ########## #


# 1. Setup ---------------------------------------------------------------------

source("scripts/0_branch_setup.R")

# 2. Set up sensor paths -------------------------------------------------------

## Sensors included in this study: 
### pH 
#### SAMI
#### Idronaut
#### EXO
#### Troll600
#### ProDSS
#### Seabird

data_path = "data/from_gdrive_240712/"
sami_path = paste0(data_path, "SAMI PH")
hydro_path = paste0(data_path, "Hydrocat")
idronaut_path = paste0(data_path, "Idronaut")
exo_path = paste0(data_path, "EXO2")
troll_path = paste0(data_path, "TROLL")
prodss_path = paste0(data_path, "Mackay_EXP19_Sensor Data.xlsx") #changed


#this is for individual sensor graphs
start_date = as.POSIXct("2024-07-08")
end_date = as.POSIXct("2024-07-31 08:30:00", tz = 'America/Los_Angeles') #this is temporary


# 3. Read in/format SAMI -------------------------------------------------------

sami_files <- list.files(sami_path, pattern = "^SAMIpH_", full.names = T)

file_lines <- read_lines(sami_files[[1]])

sami_data <- read_delim(sami_files[], delim = "\t", skip = 0)

sami_data$datetime <- as.POSIXct(paste(sami_data$DateStr, sami_data$TimeStr, format = "%Y-%m-%d %H:%M:%S"), tz='UTC')

#remove outliers (0ph measurements)
sami_data <- sami_data[sami_data$pHConstSal >= 5, ]

p <- ggplot(sami_data, aes(datetime, pHConstSal)) + 
  geom_point(size=3, alpha=0.5) + geom_line() + 
  ylab("pH") + xlab("Date") + ggtitle("Sami pH") +
  scale_x_datetime(limits = c(start_date, end_date))

p

ggsave(filename = "figures/ph_sensor_plots/sami_ph_plot.png", plot = p, width = 10, height = 6, dpi = 300)


# 4. Read/format Idronaut ------------------------------------------------------

idronaut_files <- list.files(idronaut_path, full.names = T)

idronaut_lines <- read_lines(idronaut_files[[1]])

idronaut_start <- which(grepl("^cruise end", idronaut_lines))[1]

idronaut_raw <- read_delim(idronaut_files[[1]], delim = "\t", skip = idronaut_start)

read_idronaut <- function(file){
  
  #1. Read in file line-by-line so we can parse header
  lines <- read_lines(file)
  
  #2. Junk header lines to skip
  idronaut_start <- which(grepl("^cruise end", lines))[1]
  
  #3. Read in data and clean colnames
  x <- read_delim(file, delim = ",", skip = idronaut_start, col_names = F) %>% 
    clean_names()
  
  #4. Rename columns - x# means I dunno what it is (to update later)
  colnames(x) = c("datetime_raw", "x2", "x3", "x4", "ph", "x6", "x7")
  
  #5. Initial formatting
  x %>% 
    mutate(datetime = as.POSIXct(datetime_raw, format = "%d/%m/%Y %H:%M:%S", tz = 'America/Los_Angeles'),
           ph = as.numeric(ph)) %>% 
    dplyr::select(datetime, ph)
  
}

idronaut_raw <- list.files(idronaut_path, full.names = T) %>% 
  map(read_idronaut) %>% 
  bind_rows()

idronaut_raw <- idronaut_raw %>% filter(datetime >= '2024-07-08')

#remove outliers ----
subset_data <- idronaut_raw %>% filter(datetime >= '2024-07-09')

subset_data$ph_diff_forward <- abs(subset_data$ph - lead(subset_data$ph))

indices_to_delete <- which(subset_data$ph_diff_forward > 0.03)

indices_to_exclude <- unique(unlist(sapply(indices_to_delete, function(idx) idx:(idx+10))))

subset_data <- subset_data[-indices_to_exclude, ] %>%
  dplyr::select(datetime, ph)

idronaut_raw <- rbind(idronaut_raw[1:1499, c("datetime", "ph")], subset_data)

#--------

p <- ggplot(idronaut_raw, aes(datetime, ph)) + 
  geom_point(size=1) + 
  ylim(7.75,9) + ylab("pH") + xlab("Date") + ggtitle("Idronaut pH") +
  scale_x_datetime(limits = c(start_date, end_date))

p
  
ggsave(filename = "figures/ph_sensor_plots/idronaut_ph_plot.png", plot = p, width = 10, height = 6, dpi = 300)
  

# 5. Read/format EXO -----------------------------------------------------------

exo_files <- list.files(exo_path, full.names = T)
exo_encoding <- guess_encoding(exo_files[[1]], n_max = 100) %>% slice(1) %>% pull(encoding)

## Not making a function for now, since we only have one file, but writing code
## in a way it can be functionalized easily
exo_all <- read_csv(exo_files[[1]], locale = locale(encoding = "UTF-16LE"), 
                    skip = 9, col_names = T) %>% 
  clean_names() %>% 
  mutate(datetime = parsedate::parse_date(paste(date_mm_dd_yyyy, time_hh_mm_ss))) %>% 
  mutate(ph_mean = (ph_22 + ph_23) / 2)

exo_raw <- exo_all %>% 
  dplyr::select(datetime, ph_mean, ph_22, ph_23)

#change timezone
exo_raw$datetime <- as.POSIXct(exo_raw$datetime, tz = 'America/Los_Angeles')
exo_raw$datetime <- exo_raw$datetime + hours(7)
  

## ph_22 is doing weird stuff, just use ph_23
exo_raw %>% 
  pivot_longer(cols = contains("ph")) %>% 
  ggplot(aes(datetime, value, color = name)) + 
  geom_point()

## changing NBS pH to total pH
exo_raw$ph_23 <- exo_raw$ph_23 - 0.13

#plotting the pH values
p <- ggplot(exo_raw, aes(datetime, ph_23)) +
  geom_point(size=1) +
  ylim(7.75,9.5) + ylab("pH") + xlab("Date") + ggtitle("EXO pH") +
  scale_x_datetime(limits = c(start_date, end_date)) +
  geom_line()

p

#saving pH plot
ggsave(filename = "figures/ph_sensor_plots/exo_ph_plot.png", plot = p, width = 10, height = 6, dpi = 300)


## Making an ancillary dataset for water quality
exo_wq <- read_csv(exo_files[[1]], locale = locale(encoding = "UTF-16LE"), 
                    skip = 9, col_names = T) %>% 
  clean_names() %>% 
  mutate(datetime_raw = parsedate::parse_date(paste(date_mm_dd_yyyy, time_hh_mm_ss))) %>% 
  ## Note that times are off here, looks like EXO is 8 hours behind
  mutate(datetime = datetime_raw + hours(6)) %>% ## I'd like to know the reason here, it's currently arbitrary
  mutate(ph_mean = (ph_22 + ph_23) / 2) %>% 
  dplyr::select(datetime, ph_mean, sal_psu, odo_mg_l, temp_c, turbidity_fnu, depth_m)

plot_exo_wq <- function(var){
  ggplot(exo_wq, aes(datetime, {{var}})) + 
    geom_line()
}

plot_grid(plot_exo_wq(depth_m), 
          plot_exo_wq(sal_psu), 
          ncol = 1)


# 6. Read/format Troll ---------------------------------------------------------

troll_files <- list.files(troll_path, full.names = T, pattern = ".csv")

## Not making a function for now, since we only have one file, but writing code
## in a way it can be functionalized easily
troll_lines <- read_lines(troll_files[[1]])

troll_start <- which(grepl("Date Time", troll_lines))[1]

troll_raw <- read_csv(troll_files[], skip = troll_start-1) %>% 
  clean_names() %>% 
  mutate(datetime = parsedate::parse_date(date_time)) %>% 
  rename("spcond" = specific_conductivity_m_s_cm_661921, 
         "ph" = p_h_p_h_1111222) %>% 
  dplyr::select(datetime, ph) %>%
  filter(ph >= 8)  # Filter out pH values less than 8, b/c there was some errors when we took it out of the water

#changing NBS to total
troll_raw$ph <- troll_raw$ph - 0.13

#change timezone
troll_raw$datetime <- as.POSIXct(troll_raw$datetime, tz = 'America/Los_Angeles')
troll_raw$datetime <- troll_raw$datetime + hours(7)

p <- ggplot(troll_raw, aes(datetime, ph)) + 
  geom_line() + ylim(7.5,10) + scale_x_datetime(limits = c(start_date, end_date)) +
  ylab("pH") + xlab("Date") + ggtitle("TROLL pH")

p

ggsave(filename = "figures/ph_sensor_plots/troll_ph_plot.png", plot = p, width = 10, height = 6, dpi = 300)


# 7. Read/format ProDSS --------------------------------------------------------

prodss_raw <- read_excel(prodss_path, sheet = "ProDSS") %>% 
  clean_names() %>% 
  rename("time_raw" = time) %>% 
  # Step 1: Separate am/pm from the time column
  mutate(am_pm = str_extract(time_raw, "[ap]m$"),
    time_dbl = str_remove(time_raw, "[ap]m$")) %>%
  # Step 2: If there's no ":", append ":00"
  mutate(
    time_dbl = if_else(str_detect(time_dbl, ":"),
                   time_dbl,
                   paste0(time_dbl, ":00"))) %>%
  # Step 3: Convert to time using hms or lubridate package
  mutate(time_combined = paste0(time_dbl, am_pm)) %>%
  mutate(parsed_time = parse_date_time(time_combined, orders = "I:M%p")) %>% 
  mutate(datetime = as.POSIXct(format(date, "%Y-%m-%d")) + 
           hours(hour(parsed_time)) +
           minutes(minute(parsed_time)) +
           seconds(second(parsed_time))) %>% 
  mutate(sal = as.numeric(sal), 
         temp = as.numeric(temp), 
         ph = as.numeric(p_h)) %>% 
  dplyr::select(datetime, temp, sal, ph)

#changing NBS to total
prodss_raw$ph <- prodss_raw$ph - 0.13

#time zone looks good already

p <- ggplot(prodss_raw, aes(datetime, ph)) + scale_x_datetime(limits = c(start_date, end_date)) +
  geom_point() + geom_line()

p

ggsave(filename = "figures/ph_sensor_plots/prodss_ph_plot.png", plot = p, width = 10, height = 6, dpi = 300)


# 8. Read/format HydroCAT --------------------------------------------------------

hydro_files <- list.files(hydro_path, full.names = T)

cat_data <- read_csv(hydro_files)

cat_data <- cat_data %>% rename(pH = `pH (pH)`, Celsius = `Temperature (Celsius)`, datetime = `DateTime (UTC-07:00)`)

#It says that it's in UTC, but it only lines up if I import it in PDT
cat_data$datetime <- as.POSIXct(cat_data$datetime, format = "%m/%d/%Y %H:%M:%S", tz='America/Los_Angeles')

#changing NBS to total
cat_data$pH <- cat_data$pH - 0.13

p <- ggplot(cat_data, aes(datetime, pH)) + geom_line() +
  geom_point(size=3, alpha=0.5) + ylab("pH") + xlab("Date") + ggtitle("HydroCAT pH") +
  scale_x_datetime(limits = c(start_date, end_date))

p

ggsave(filename = "figures/ph_sensor_plots/hydrocat_ph_plot.png", plot = p, width = 10, height = 6, dpi = 300)


# 9. Combine pH datasets and plot ----------------------------------------------

## Clean up datasets
cat_raw <- cat_data %>% 
  clean_names() 

sami_raw <- sami_data %>% 
  clean_names() %>% 
  rename("ph" = p_h_const_sal)



ph_combined <- bind_rows(cat_raw %>% rename("ph" = p_h) %>% dplyr::select(datetime, ph) %>% mutate(sensor = "HydroCAT"), 
                         prodss_raw %>% dplyr::select(datetime, ph) %>% mutate(sensor = "ProDSS"), 
                         idronaut_raw %>% mutate(sensor = "Idronaut"), 
                         sami_raw %>% dplyr::select(datetime, ph) %>% mutate(sensor = "SAMI"),
                         exo_raw %>% rename("ph" = ph_23) %>% dplyr::select(datetime, ph) %>% mutate(sensor = "EXO2"))
                         #troll_raw %>% dplyr::select(datetime, ph) %>% mutate(sensor = "TROLL"))

p_ph <- ph_combined %>% 
  filter(datetime > "2024-07-08") %>%
  filter(datetime < "2024-07-31") %>%
  ggplot(aes(datetime, ph, color = sensor)) + 
  geom_line() 

ggplotly(p_ph)

ggsave("figures/ph_comparison.png", width = 8, height = 4)


# 9.1. Clean pH plot -----------------------------------------------------------

ph_filtered <- ph_combined %>% 
  filter(sensor != "TROLL") %>% 
  filter(datetime > "2024-07-08") %>%
  filter(datetime < "2024-07-23") 

ph_colors = PNWColors::pnw_palette("Bay", n = length(unique(ph_filtered$sensor)))

ph_filtered %>% 
  mutate(datetime_hr = round_date(datetime, "1 hours")) %>% 
  #mutate(hour = as_date(datetime)) %>% 
  group_by(datetime_hr, sensor) %>%
  summarize(mean_ph = mean(ph, na.rm = T),
            sd_ph = sd(ph, na.rm = T)) %>%
  ggplot(aes(x = datetime_hr, y = mean_ph, color = sensor)) + 
  geom_hline(yintercept = 8, color = "gray", linetype = "dashed") + 
  geom_hline(yintercept = 9, color = "gray", linetype = "dashed") + 
  geom_point(size = 2, alpha = 0.4, show.legend = T) +
  geom_line(aes(y = mean_ph), show.legend = T) + 
  labs(x = "Date", y = "pH", color = "Sensor") + 
  scale_color_manual(values = ph_colors) 
ggsave("figures/240730_ph_comparison_v2.png", width = 6, height = 4)


# 10. Export pH dataset --------------------------------------------------------
write_csv(ph_combined, "data/ph_data_raw.csv")


# 11. Make and export wq dataset -----------------------------------------------
## Since we already have the EXO data imported, we'll also make a separate file
## for water quality, since we need temperature and salinity for seacarb

wq <- exo_all %>% 
  select(datetime, temp_c, sal_psu, odo_mg_l, depth_m, ph_mean, chlorophyll_rfu, turbidity_fnu)
write_csv(wq, "data/wq_exo.csv")


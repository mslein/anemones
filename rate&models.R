pacman::p_load(rTPC, nls.multstart, broom, tidyverse, readxl, lubridate, timetk, respR, plyr, minpack.lm)

#i made this nifty function which will take all the sheets, and turn them into dfs:)
sheet_function <- function(path, df_name){
  sheetnames <- excel_sheets(path)[6:15]
  mylist <- lapply(sheetnames, function(x) 
    read_excel(path,x, col_names = TRUE))
  # name the dataframes
  names(mylist) <- sheetnames
  #use Map to bind all the elements of the list into a dataframe
  my_list <- Map(cbind, mylist, Cluster = names(mylist))
  df <- do.call("rbind", my_list)
  #name the dataframes uniquely
  assign(x = df_name, value = df, envir = globalenv())
}

#doing this for all the sheets i have from my experiment
sheet_function("./bmsc exp 2024/full experiment/14aug/14aug6c.xlsx", df_name = "pr6c")
sheet_function("./bmsc exp 2024/full experiment/15aug12c/12c_PR+R_15aug2024.xlsx", df_name = "pr12c")
sheet_function("./bmsc exp 2024/full experiment/16aug18c/16aug18c.xlsx", df_name = "pr18c")
sheet_function("./bmsc exp 2024/full experiment/17aug24c/24c_PR+R_17aug2024.xlsx", df_name = "pr24c")
sheet_function("./bmsc exp 2024/full experiment/19aug27c/27c_PR+R_19aug2024.xlsx", df_name = "pr27c")
sheet_function("./bmsc exp 2024/full experiment/21aug34c/30c_PR+R_20aug2024.xlsx", df_name = "pr30_34c")
sheet_function("./bmsc exp 2024/full experiment/22aug18c/18c_PR+R_22aug2024.xlsx", df_name = "pr18c_redo")
sheet_function("./bmsc exp 2024/full experiment/23aug12c/12c_PR+R_23aug2024.xlsx", df_name = "pr12c_redo")
sheet_function("./bmsc exp 2024/full experiment/21aug34c/6c_PR+R_24aug2024.xlsx", df_name = "pr6c_redo")


#denoting which times should be counted as photosynthesis and respiration for each date + anemone
all_data <- rbind(pr6c, pr12c, pr18c, pr24c, pr27c, pr30_34c, pr18c_redo, pr12c_redo, pr6c_redo) %>%
  #mutate(Date=as.Date(Date)) %>%
  mutate(anemone = case_when(Cluster == "Ch 1" ~ 1, 
                             Cluster == "Ch 2" ~ 2, 
                             Cluster == "Ch 3" ~ 3, 
                             Cluster == "Ch 4" ~ 4, 
                             Cluster == "Ch 5" ~ 5, 
                             Cluster == "Ch 6" ~ 6, 
                             Cluster == "Ch 7" ~ 7, 
                             Cluster == "Ch 8" ~ 8, 
                             Cluster == "Ch 9" ~ 9, 
                             Cluster == "Ch 10" ~ 10)) %>%
  mutate(response_type = case_when(anemone %in% c(1:10) & Date >= ymd_hms('2024-08-14 15:25:00') & Date <= ymd_hms('2024-08-14 17:24:00') ~ "respiration", 
                                   anemone %in% c(1:10) & Date >= ymd_hms('2024-08-14 17:25:00') & Date <= ymd_hms('2024-08-14 19:11:00') ~ "photosynthesis",
                                   anemone %in% c(1:10) & Date >= ymd_hms('2024-08-15 11:41:00') & Date <= ymd_hms('2024-08-15 13:42:00') ~ "respiration", 
                                   anemone %in% c(1:10) & Date >= ymd_hms('2024-08-15 13:43:00') & Date <= ymd_hms('2024-08-15 15:30:00') ~ "photosynthesis",
                                   anemone %in% c(1:10) & Date >= ymd_hms('2024-08-16 11:10:00') & Date <= ymd_hms('2024-08-16 12:00:00') ~ "respiration", 
                                   anemone %in% c(1:10) & Date >= ymd_hms('2024-08-16 12:01:00') & Date <= ymd_hms('2024-08-16 15:33:00') ~ "photosynthesis",
                                   anemone %in% c(9) & Date >= ymd_hms('2024-08-17 12:54:00') & Date <= ymd_hms('2024-08-17 13:05:00') ~ "respiration",
                                   anemone %in% c(2) & Date >= ymd_hms('2024-08-17 12:54:00') & Date <= ymd_hms('2024-08-17 13:14:00') ~ "respiration",
                                   anemone %in% c(5) & Date >= ymd_hms('2024-08-17 12:54:00') & Date <= ymd_hms('2024-08-17 13:15:00') ~ "respiration",
                                   anemone %in% c(1,3,4,6,7,8,10) & Date >= ymd_hms('2024-08-17 12:54:00') & Date <= ymd_hms('2024-08-17 13:23:00') ~ "respiration",
                                   anemone %in% c(1:10) & Date >= ymd_hms('2024-08-17 13:42:00') & Date <= ymd_hms('2024-08-17 14:45:00') ~ "photosynthesis",
                                   anemone %in% c(1:10) & Date >= ymd_hms('2024-08-19 15:03:00') & Date <= ymd_hms('2024-08-19 15:33:00') ~ "respiration", 
                                   anemone %in% c(1:10) & Date >= ymd_hms('2024-08-19 15:34:00') & Date <= ymd_hms('2024-08-19 16:43:00') ~ "photosynthesis",
                                   anemone %in% c(1:10) & Date >= ymd_hms('2024-08-20 16:42:00') & Date <= ymd_hms('2024-08-20 16:51:00') ~ "respiration", #probably exclude channel 2
                                   anemone %in% c(1:10) & Date >= ymd_hms('2024-08-20 16:52:00') & Date <= ymd_hms('2024-08-20 18:05:00') ~ "photosynthesis",
                                   anemone %in% c(1:10) & Date >= ymd_hms('2024-08-21 12:08:00') & Date <= ymd_hms('2024-08-21 12:14:00') ~ "respiration", 
                                   anemone %in% c(1:10) & Date >= ymd_hms('2024-08-21 12:15:00') & Date <= ymd_hms('2024-08-21 12:33:00') ~ "photosynthesis")) %>%
  drop_na(response_type) #getting rid of all the extra times that were not periods of photosynthesis and respiration

pr30_34c %>%
  ggplot(aes(x=Date, y=Oxygen))+
  geom_point()+
  facet_wrap(~Cluster)



#there is something weird about the 6c data


#separating date and time to group by date
all_data2 <- all_data %>% separate(Date, into = c("date", "time"), sep = " (?=[^ ]+$)") %>% mutate(date=as.Date(date))

all_data2 %>%
  filter(date=="2024-08-20") %>%
  ggplot(aes(x=time, y=Oxygen, colour=response_type))+
  geom_point()+
  facet_wrap(~Cluster)



#creating a group dataframe
all_data_grp <- all_data2 %>%
  group_by(date, anemone, response_type) %>%
  filter(date !="2024-08-21" & anemone != 2 | 5) %>%
  filter(date !="2024-08-20" & anemone != 2) %>%
  filter(anemone != 1 | 8)


mean_temp_data <- all_data2 %>%
  group_by(date, anemone) %>%
  dplyr::summarize(mean_temp=mean(Temperature)) %>%
  mutate(date=as.Date(date), 
         anemone=as.factor(anemone))

# get group keys
group_name_df <- group_keys(all_data_grp) %>%
  mutate(group_name = str_c(date," ",anemone," ",as.character(response_type)))

# get name for each group
group_name <- group_name_df$group_name

#function i made to gets rates
check_fit <- function(df){
  max <- max(df$Oxygen)
  min <- min(df$Oxygen)
  
  rate <- df %>% 
    inspect(4, 8) %>%
    calc_rate(from = max, to = min, by = "O2") %>%
    summary()
}



df_list <- group_split(all_data_grp) %>%
  setNames(group_name) %>% # assign name to each split table
  lapply(check_fit)

df <- as.data.frame(do.call(cbind, df_list)) %>%
  tibble::rownames_to_column("value") %>%
  filter(value == "rate") %>%
  mutate_all(as.character) %>% 
  pivot_longer(cols=everything(), names_to = "measurement", values_to = "rate") %>%
  slice(-1) %>%
  separate(measurement, into = c("date", "anemone", "response_type"), sep = " ") %>%
  mutate(date=as.Date(date), 
         anemone=as.factor(anemone)) %>%
  mutate(clean = case_when(date=="2024-08-21" & anemone %in% c(2,5) ~ "exclude", 
                           date=="2024-08-20" & anemone %in% c(2) ~ "exclude",
                           TRUE ~ "retain")) %>%
  filter(clean=="retain")

#weights 
wt <- read_csv("./bmsc exp 2024/full experiment/wetweights.csv") %>% mutate(date=as.Date(date), 
                                                                            anemone=as.factor(anemone))
#joining the mean temp and weights to this rates to get a big dataframe
df_full <- left_join(df, mean_temp_data, by= c("date", "anemone")) %>%
  left_join(wt, by=c("date", "anemone"))

#since 1 and 8 were blanks, they don't have weights, so dropping them out for this preliminary run
df_full_anems <- df_full %>% mutate(rate=as.numeric(rate)) %>% 
  pivot_wider(names_from = response_type, values_from = rate) %>%
  mutate(gross_photosynthesis = photosynthesis-respiration, 
         respiration_pos = respiration*-1) %>%
  pivot_longer(col= c(gross_photosynthesis, respiration_pos), names_to = "response_type", values_to = "rate") %>%
  mutate(rate2=rate/blotted_wet_wet_g) %>%
  drop_na(rate2)





# now trying to calculate slopes and fits 
# borrowing this code from keila 
fit_and_predict <- function(data) {
  
  temp_data <- data %>%
    mutate(Temperature = as.numeric(mean_temp), 
           anemone=as.factor(anemone)) # just in case integer causes problems
  
  # Get start values and limits
  start_vals <- get_start_vals(temp_data$Temperature, temp_data$rate2, model_name = 'sharpeschoolhigh_1981')
  low_lims <- get_lower_lims(temp_data$Temperature, temp_data$rate2, model_name = 'sharpeschoolhigh_1981')
  upper_lims <- get_upper_lims(temp_data$Temperature, temp_data$rate2, model_name = 'sharpeschoolhigh_1981')
  
  # Fit the model
  fit <- nls_multstart(rate2 ~ sharpeschoolhigh_1981(temp = Temperature, r_tref, e, eh, th, tref = 15),
                       data = temp_data,
                       iter = 500,
                       start_lower = start_vals - 10,
                       start_upper = start_vals + 10,
                       lower = low_lims,
                       upper = upper_lims,
                       supp_errors = 'Y')
  
  # Generate new data for predictions using augment
  new_data <- data.frame(Temperature = seq(min(temp_data$Temperature), max(temp_data$Temperature), 0.5))
  preds <- broom::augment(fit, newdata = new_data)
  
  # Add species information to the predictions
  preds <- preds %>%
    mutate(response_type= unique(temp_data$response_type))
  
  return(list(data = temp_data, preds = preds))
}


# Apply the function to each anemone and response type
results <- df_full_anems %>%
  group_by(response_type) %>%
  group_split() %>%
  lapply(fit_and_predict)

# Combine results
combined_data <- do.call(rbind, lapply(results, function(x) x$data))
combined_preds <- do.call(rbind, lapply(results, function(x) x$preds))

plot_data <- bind_rows(
  combined_data %>% mutate(Type = "Observed"),
  combined_preds %>% mutate(Type = "Fitted")
)


# Plot Thermal Performance Curves with Facets
ggplot(plot_data, aes(x = Temperature, y = rate2, color=response_type)) +
  #scale_color_manual(values=c( "#a559aa", "#59a89c", "#f0c571", "#ae282c","#082a54" ))+
  geom_point(data = subset(plot_data, Type == "Observed"), alpha=0.5) +
  geom_line(data = subset(plot_data, Type == "Fitted"), aes(y=.fitted)) +
  facet_wrap(~response_type) +
  theme_classic() 
#labs(x = 'Temperature (ºC)', y = 'Dispersal rate at leading edge', color = 'Species') +
#theme(legend.position = "bottom")


df_full_anems %>%
  ggplot(aes(x=log(blotted_wet_wet_g), y=log(rate2), colour=response_type))+
  geom_point()+
  facet_wrap(~response_type)+
  geom_smooth(method="lm")

ps <- df_full_anems %>% filter(response_type == "gross_photosynthesis") %>% mutate(temp=mean_temp)
r <- df_full_anems %>% filter(response_type == "respiration_pos")%>% mutate(temp=mean_temp)
lm(rate2 ~ blotted_wet_wet_g, data=r)

k <- .000086173303
Te <- mean_te

w <- df_full_anems %>%
  mutate(Te=mean_temp+273.15,
         activ_en = (log(rate2/((blotted_wet_wet_g)^(3/4))))*k*Te) %>%
  mutate(response_type=as.factor(response_type))

w %>%
  group_by(response_type) %>%
  dplyr::summarize(mean_ea=mean(activ_en, na.rm =TRUE))

w %>%
  ggplot(aes(x=response_type, y=activ_en, fill=response_type))+
  geom_boxplot()+
  geom_point(alpha=0.5)









########trying to do some bootstrapping


# Get start values and limits
start_vals <- get_start_vals(r$temp, r$rate2, model_name = 'sharpeschoolhigh_1981')
low_lims <- get_lower_lims(r$temp, r$rate2, model_name = 'sharpeschoolhigh_1981')
upper_lims <- get_upper_lims(r$temp, r$rate2, model_name = 'sharpeschoolhigh_1981')

# Fit the model
fit <- nls_multstart(rate2 ~ sharpeschoolhigh_1981(temp = temp, r_tref, e, eh, th, tref = 15),
                     data = r,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')
nlstools::confint2(fit)

summary
# Generate new data for predictions using augment
new_data <- data.frame(Temperature = seq(min(temp_data$temp), max(temp_data$temp), 0.5))
preds <- broom::augment(fit, newdata = new_data)

# Add species information to the predictions
preds <- preds %>%
  mutate(response_type= unique(temp_data$response_type))

# refit model using nlsLM
fit_nlsLM2 <- nlsLM(rate2~sharpeschoolhigh_1981(temp = Temperature, r_tref,e,eh,th, tref = 15),
                    data = temp_data,
                    start = coef(fit$sharpeschoolhigh[[1]]),
                    lower = get_lower_lims(temp_data$Temperature, temp_data$rate2, model_name = 'sharpeschoolhigh_1981'),
                    upper = get_upper_lims(temp_data$Temperature, temp_data$rate2, model_name = 'sharpeschoolhigh_1981'),
                    control = nls.lm.control(maxiter=500),
                    weights = rep(1, times = nrow(temp_data)))

# bootstrap using case resampling
boot2 <- Boot(fit_nlsLM2, method = 'case')

d_preds <- select(d_fit, preds) %>%
  unnest(preds)

# predict over new data
boot2_preds <- boot2$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(Temperature = seq(min(temp_data$Temperature), max(temp_data$Temperature), length.out = 100))) %>%
  ungroup() %>%
  mutate(pred = sharpeschoolhigh_1981(Temperature, r_tref, e, eh, th, tref = 15))

# calculate bootstrapped confidence intervals
boot2_conf_preds <- group_by(boot2_preds, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975)) %>%
  ungroup()

return(list(data = temp_data, preds = preds, boot = boot2_preds, boot_ci=boot2_conf_preds))
}


# Apply the function to each anemone and response type
results <- df_full_anems %>%
  group_by(response_type) %>%
  group_split() %>%
  lapply(bootstrapping)

# Combine results
combined_data <- do.call(rbind, lapply(results, function(x) x$data))
combined_preds <- do.call(rbind, lapply(results, function(x) x$preds))
combined_boot_preds <- do.call(rbind, lapply(results, function(x) x$boot2_preds))
combined_boots_cis <- do.call(rbind, lapply(results, function(x) x$boot2_conf_preds))

plot_data <- bind_rows(
  combined_data %>% mutate(Type = "Observed"),
  combined_preds %>% mutate(Type = "Fitted")
)










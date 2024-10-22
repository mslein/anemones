#install.packages("pacman")
library(pacman)
pacman::p_load(rTPC, nls.multstart, broom, tidyverse)



tpc <- read_csv("bmsc exp 2024/summary_13augramping.csv") %>%
  mutate(slope_correct = case_when(response == "respiration" ~ slope*-1, 
                                   response == "photosynthesis" ~slope*1), 
         anemone=as.factor(anemone)) %>%
  rename(rate=slope_correct, 
         Temperature=mean_temp) %>%
  drop_na(rate) %>%
  select(-Temp)

tpc_ps <- tpc %>% filter(response == "photosynthesis")
tpc_r <- tpc %>% filter(response == "respiration")


fit_and_predict <- function(data) {
  
  temp_data <- data %>%
    mutate(Temperature = as.numeric(Temperature), 
           anemone=as.factor(anemone)) # just in case integer causes problems
  
  # Get start values and limits
  start_vals <- get_start_vals(temp_data$Temperature, temp_data$rate, model_name = 'sharpeschoolhigh_1981')
  low_lims <- get_lower_lims(temp_data$Temperature, temp_data$rate, model_name = 'sharpeschoolhigh_1981')
  upper_lims <- get_upper_lims(temp_data$Temperature, temp_data$rate, model_name = 'sharpeschoolhigh_1981')
  
  # Fit the model
  fit <- nls_multstart(rate ~ sharpeschoolhigh_1981(temp = Temperature, r_tref, e, eh, th, tref = 15),
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
    mutate(response_type = unique(temp_data$response_type))
  
  return(list(data = temp_data, preds = preds))
}

# Apply the Function to Each Species
results_r <- tpc_r %>%
  group_by(as.factor(anemone)) %>%
  group_split() %>%
  lapply(fit_and_predict)

# Combine results
combined_data_r <- do.call(rbind, lapply(results_r, function(x) x$data))
combined_preds_r <- do.call(rbind, lapply(results_r, function(x) x$preds))

plot_data_r <- bind_rows(
  combined_data_r %>% mutate(Type = "Observed"),
  combined_preds_r %>% mutate(Type = "Fitted")
)


# Plot Thermal Performance Curves with Facets
ggplot(plot_data_r, aes(x = Temperature, y = rate, color=anemone)) +
  #scale_color_manual(values=c( "#a559aa", "#59a89c", "#f0c571", "#ae282c","#082a54" ))+
  geom_point(data = subset(plot_data_r, Type == "Observed")) +
  geom_line(data = subset(plot_data_r, Type == "Fitted"), aes(y=.fitted)) +
  facet_wrap(~anemone, scales="free_y") +
  theme_classic() +
  labs(x = 'Temperature (ºC)', y = 'Dispersal rate at leading edge', color = 'Species') +
  theme(legend.position = "bottom")

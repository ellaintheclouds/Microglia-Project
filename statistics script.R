# Creating subsets of the data that I want to look at---------------------------
subset_list <- list(
  # Splitting into cortex and striatum
  "ctx" = data[data$region == "ctx", "percent_area_adjusted"],
  "cpu" = data[data$region == "cpu", "percent_area_adjusted"],
  
  # Splitting by diet (within cortex and striatum groups)
  "control_ctx" = 
    data[data$diet == "C/C" & data$region == "ctx", "percent_area_adjusted"],
  "test_ctx" = 
    data[data$diet == "HF/C" & data$region == "ctx", "percent_area_adjusted"],
  "control_cpu" = 
    data[data$diet == "C/C" & data$region == "cpu", "percent_area_adjusted"],
  "test_cpu" = 
    data[data$diet == "HF/C" & data$region == "cpu", "percent_area_adjusted"],
  
  # Splitting by sex (within cortex and striatum groups)
  "male_ctx" = 
    data[data$sex == "male" & data$region == "ctx", "percent_area_adjusted"],
  "female_ctx" = 
    data[data$sex == "female" & data$region == "ctx", "percent_area_adjusted"],
  "male_cpu" = 
    data[data$sex == "male" & data$region == "cpu", "percent_area_adjusted"],
  "female_cpu" = 
    data[data$sex == "female" & data$region == "cpu", "percent_area_adjusted"],
  
  "control_male_ctx" = data[data$diet == "C/C" & data$sex == "male" & 
                              data$region == "ctx", "percent_area_adjusted"],
  "control_female_ctx" = data[data$diet == "C/C" & data$sex == "female" & 
                                data$region == "ctx", "percent_area_adjusted"],
  "test_male_ctx" = data[data$diet == "HF/C" & data$sex == "male" & 
                           data$region == "ctx", "percent_area_adjusted"],
  "test_female_ctx" = data[data$diet == "HF/C" & data$sex == "female" &
                             data$region == "ctx", "percent_area_adjusted"],
  "control_male_cpu" = data[data$diet == "C/C" & data$sex == "male" & 
                              data$region == "cpu", "percent_area_adjusted"],
  "contol_female_cpu" = data[data$diet == "C/C" & data$sex == "female" & 
                               data$region == "cpu", "percent_area_adjusted"],
  "test_male_cpu" = data[data$diet == "HF/C" & data$sex == "male" & 
                           data$region == "cpu", "percent_area_adjusted"],
  "test_female_cpu" = data[data$diet == "HF/C" & data$sex == "female" & 
                             data$region == "cpu", "percent_area_adjusted"]
)

# Finding basic statistics and assessing normality------------------------------
# Creating a new data frame for statistical results
stats_df <- data.frame(matrix(ncol = 4, nrow = length(subset_list)))
colnames(stats_df) <- c("test", "mean", "se", "shapiro_result")
View(stats_df)

# Finding means, standard deviations, and normality of data subsets
se <- function(x) sd(x)/sqrt(length(x)) # creating a standard error function

for(i in 1:length(subset_list)){
  name <- subset_list[i]|> names()
  subset <- subset_list[[i]]
  subset_df <- data.frame(subset)
  
  stats_df[[1]][i] <- name
  stats_df[[2]][i] <- subset |> mean()
  stats_df[[3]][i] <- subset |> se()
  shapiro <- subset |> shapiro.test()
  
  if(shapiro$p.value>0.05){
    stats_df[[4]][i] <- "normal"
  } else{
    stats_df[[4]][i] <- "not normal"
  }
  
  colnames(subset_df) <- "percent" # (allows ggplot to plot varying columns)
  
  plot <- # Creating plots to display the distribution of the subset data
    ggplot(data = subset_df, mapping = aes(x = percent)) + 
    geom_density(colour = "cornflowerblue", 
                 fill = alpha("cornflowerblue", 0.3)) +
    theme_minimal() + 
    ylab("Density") + xlab("Area Covered by Microglia %")
  #ggsave(plot = plot, filename = paste0("Output/Graphs/distribution density",  #
   #name, ".png"), width = 6.25, height = 5)
}

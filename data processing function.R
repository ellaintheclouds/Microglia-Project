data <- read.csv("Input/microglia_data.csv")

# Adding in extra columns for brain region and animal number
data$region <- NA
data$animal <- NA

data$split <- data$file_name |>
  strsplit(split = " ")

for(i in 1:nrow(data)){
  data$animal[[i]] <- data$split[[i]][1]
  data$region[[i]] <- data$split[[i]][2]
}

# Adding in extra columns for sex and diet
data$sex <- NA
data$diet <- NA

for(i in 1:nrow(data)){
  if(data$animal[[i]] == "PC44" | data$animal[[i]] == "PC45" | 
     data$animal[[i]] == "PC71"){
    data$sex[[i]] <- "male"
    data$diet[[i]] <- "C/C"
  }
  else if(data$animal[[i]] == "PC1" | data$animal[[i]] == "PC33" | 
          data$animal[[i]] == "PC40"){
    data$sex[[i]] <- "male"
    data$diet[[i]] <- "HF/C"
  }
  else if(data$animal[[i]] == "PC20" | data$animal[[i]] == "PC49" | 
          data$animal[[i]] == "PC70"){
    data$sex[[i]] <- "female"
    data$diet[[i]] <- "C/C"
  }
  else if(data$animal[[i]] == "PC6" | data$animal[[i]] == "PC32" | 
          data$animal[[i]] == "PC57"){
    data$sex[[i]] <- "female"
    data$diet[[i]] <- "HF/C"
  }
}

# Quantifying variability in light between the two microscopy sessions
day_1_control <- data[data$file_name == 
                        "PC44 ctx x10 a - day 2.jpg",]["percent_area"] |> 
  as.numeric()

day_2_control <- data[data$file_name == "PC44 ctx x10 a.jpg",]["percent_area"]|> 
  as.numeric()

session_variability <- day_2_control/day_1_control

# Removing the calibration duplicate data point
data <- data[-27,]
rownames(data) <- c(1:48)

# Adjusting % coverage from session 1 to be comparable to session 2
for(i in 1:nrow(data)){
  
  if(data$animal[[i]] == "PC40" | data$animal[[i]] == "PC44" | 
     data$animal[[i]] == "PC71"){
    
    unadjusted <- as.numeric(data$percent_area[[i]])
    data$percent_area_adjusted[[i]] <- unadjusted*session_variability
  }
  else{
    data$percent_area_adjusted[[i]] <- data$percent_area[[i]]
  }
}
data$percent_area_adjusted <- as.numeric(data$percent_area_adjusted)

# Creating a new data frame to export and include in paper
export_df <- data[c("animal", "diet", "sex", "region", 
                    "percent_area_adjusted")] |> 
  write.csv("Output/processed data for paper.csv", row.names = FALSE)                  #

# Replacing data with the mean
df <- data.frame(matrix(ncol = 12, nrow = 24))
colnames(df) <- colnames(data)

for(i in 1:24){
  df[i,] <- data[2*i,]
  df[i,"percent_area_adjusted"] <- (data[2*i, "percent_area_adjusted"] + 
                                      data[(2*i)-1, "percent_area_adjusted"])/2
}
View(data)
View(df)

data <- df
data <- data[c("animal", "region", "sex", "diet", 
              "percent_area_adjusted")]

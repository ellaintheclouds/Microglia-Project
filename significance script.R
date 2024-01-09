# One-way significance Testing -------------------------------------------------
# One way testing: diet, given region (non-parametric)
leveneTest(percent_area_adjusted ~ diet, 
           data = data[data$region == "ctx",]) # p = 0.2766 equal variance
wilcox.test(subset_list[["control_ctx"]], subset_list[["test_ctx"]], 
            alternative = "two.sided", exact = FALSE) # p-value = 0.02255 *

leveneTest(percent_area_adjusted ~ diet, 
           data = data[data$region == "cpu",]) # p = 0.7433 equal variance
wilcox.test(subset_list[["control_cpu"]], subset_list[["test_cpu"]], 
            alternative = "two.sided", exact = FALSE) # p-value = 0.06896 NS

# One way testing: sex, given region (non-parametric)
leveneTest(percent_area_adjusted ~ sex, 
           data = data[data$region == "ctx",]) # p = 0.589 equal variance 
wilcox.test(subset_list[["female_ctx"]], subset_list[["male_ctx"]], 
            alternative = "two.sided", exact = FALSE) # p-value = 0.01529 *

leveneTest(percent_area_adjusted ~ sex, 
           data = data[data$region == "cpu",]) # p = 0.7737 equal variance 
wilcox.test(subset_list[["female_cpu"]], subset_list[["male_cpu"]], 
            alternative = "two.sided", exact = FALSE) # p-value = 0.02258 *

# One way testing: region (non-parametric)
leveneTest(percent_area_adjusted ~ region, 
           data = data) # p = 0.6869 equal variance
wilcox.test(subset_list[["ctx"]], subset_list[["cpu"]], 
            alternative = "two.sided", exact = FALSE) # p-value = 0.7649 NS


# Two-way significance testing--------------------------------------------------
# Two way testing: diet and sex, given region:
leveneTest(percent_area_adjusted ~ diet*sex, 
           data = data[data$region ==  "ctx",]) # p = 0.02996 * unequal variance
twoway_test_sex_ctx <- aov(percent_area_adjusted ~ diet+sex, 
                           data = data[data$region ==  "ctx",])
summary(twoway_test_sex_ctx)
#             Df Sum Sq Mean Sq F value Pr(>F)   
#diet         1  205.2  205.18   9.524 0.0056 **
#sex          1  172.3  172.30   7.998 0.0101 * 
#Residuals   21  452.4   21.54 
twoway_test_sex_ctx_interaction <- aov(percent_area_adjusted ~ diet*sex, 
                                       data = data[data$region ==  "ctx",])
summary(twoway_test_sex_ctx_interaction)
#             Df Sum Sq Mean Sq F value  Pr(>F)   
#diet         1  205.2  205.18   9.203 0.00656 **
#sex          1  172.3  172.30   7.728 0.01156 * 
#diet:sex     1    6.5    6.48   0.291 0.59563   
#Residuals   20  445.9   22.30  

leveneTest(percent_area_adjusted ~ diet*sex, 
           data = data[data$region ==  "cpu",]) # p = 0.18 equal variance
twoway_test_sex_cpu <- aov(percent_area_adjusted ~ diet+sex, 
                           data = data[data$region ==  "cpu",])
summary(twoway_test_sex_cpu)
#             Df Sum Sq Mean Sq F value Pr(>F)  
#diet         1  139.8  139.78   4.162 0.0541 .
#sex          1  219.9  219.94   6.548 0.0183 *
#Residuals   21  705.3   33.59    
twoway_test_sex_cpu_interaction <- aov(percent_area_adjusted ~ diet*sex, 
                                       data = data[data$region ==  "cpu",])
summary(twoway_test_sex_cpu_interaction)
#             Df Sum Sq Mean Sq F value Pr(>F)  
#diet         1  139.8  139.78   4.272 0.0519 .
#sex          1  219.9  219.94   6.721 0.0174 *
#diet:sex     1   50.9   50.87   1.555 0.2269  
#Residuals   20  654.5   32.72    

# Two way testing: diet and region:
leveneTest(percent_area_adjusted ~ diet*region, 
           data = data) # p = 0.6656 equal variance
twoway_test_region <- aov(percent_area_adjusted ~ diet+region, data = data)
summary(twoway_test_region)
#             Df Sum Sq Mean Sq F value  Pr(>F)   
#diet         1  341.8   341.8   9.904 0.00292 **
#region       1    8.1     8.1   0.235 0.63017   
#Residuals   45 1553.1    34.5  
twoway_test_region_interaction <- aov(percent_area_adjusted ~ diet*region, 
                                      data = data)
summary(twoway_test_region_interaction)
#             Df Sum Sq Mean Sq F value  Pr(>F)   
#diet         1  341.8   341.8   9.704 0.00323 **
#region       1    8.1     8.1   0.230 0.63370   
#diet:region  1    3.1     3.1   0.089 0.76710   
#Residuals   44 1550.0    35.2 

# Three way testing: diet, sex and region
leveneTest(percent_area_adjusted ~ diet*region*sex, 
           data = data) # p = 0.06312 equal variance
threeway_test <- aov(percent_area_adjusted ~ diet+region+sex, data = data)
summary(threeway_test)
#             Df Sum Sq Mean Sq F value   Pr(>F)    
#diet         1  341.8   341.8  12.940 0.000810 ***
#region       1    8.1     8.1   0.307 0.582291    
#sex          1  390.8   390.8  14.793 0.000383 ***
#Residuals   44 1162.3    26.4  

threeway_test_interaction <- aov(percent_area_adjusted ~ diet*region*sex, 
                                 data = data)
summary(threeway_test_interaction)
#                 Df Sum Sq Mean Sq F value   Pr(>F)    
#diet             1  341.8   341.8  12.426 0.001077 ** 
#region           1    8.1     8.1   0.295 0.590133    
#sex              1  390.8   390.8  14.205 0.000529 ***
#diet:region      1    3.1     3.1   0.114 0.737715    
#diet:sex         1   46.8    46.8   1.703 0.199393    
#region:sex       1    1.5     1.5   0.053 0.819473    
#diet:region:sex  1   10.5    10.5   0.382 0.539913    
#Residuals       40 1100.4    27.5  

# Comparing different models created by ANOVA to see which is the best fit
model_set <- list(twoway_test_sex_ctx, twoway_test_sex_ctx_interaction,
                  twoway_test_sex_cpu, twoway_test_sex_cpu_interaction,
                  twoway_test_region, twoway_test_region_interaction, 
                  threeway_test, threeway_test_interaction)

model_names <- c("twoway_test_sex_ctx", "twoway_test_sex_ctx_interaction",
                 "twoway_test_sex_cpu", "twoway_test_sex_cpu_interaction",
                 "twoway_test_region", "twoway_test_region_interaction",
                 "threeway_test", "threeway_test_interaction")

aictab(model_set, modnames = model_names) 
#                               K   AICc Delta_AICc AICcWt Cum.Wt      LL
#twoway_test_sex_ctx             4 148.69       0.00   0.80   0.80  -69.29
#twoway_test_sex_ctx_interaction 5 151.57       2.88   0.19   0.99  -69.12
#twoway_test_sex_cpu             4 159.35      10.66   0.00   1.00  -74.62
#twoway_test_sex_cpu_interaction 5 160.78      12.09   0.00   1.00  -73.72
#threeway_test                   5 300.62     151.93   0.00   1.00 -144.60
#threeway_test_interaction       9 309.30     160.61   0.00   1.00 -143.28
#twoway_test_region              4 312.03     163.34   0.00   1.00 -151.55
#twoway_test_region_interaction  5 314.44     165.75   0.00   1.00 -151.50


# Post-hoc test and correction of significance values---------------------------
data$sex_diet <- NA
data$region_diet <- NA
data$sex_region_diet <- NA

for(i in 1:nrow(data)){ # Adding extra column to allow multiple comparisons 
  data$sex_diet[[i]] <- paste0(data$sex[[i]], data$diet[[i]]) |>
    as.character()
  data$region_diet[[i]] <- paste0(data$region[[i]], data$diet[[i]]) |>
    as.character()
  data$sex_region_diet[[i]] <- paste0(data$sex[[i]], data$region[[i]], 
                                      data$diet[[i]]) |> 
    as.character()
}

# Two way testing: diet and sex, given region:
pairwise.wilcox.test(data[data$region == "ctx",]$percent_area_adjusted,  
                     data[data$region == "ctx",]$sex_diet, paired = FALSE,
                     p.adj = "bonferroni",
                     alternative = "two.sided", 
                     exact = FALSE)
#           femaleC/C femaleHF/C maleC/C
#femaleHF/C 1.00      -          -      
#maleC/C    0.03      0.03       -      
#maleHF/C   1.00      1.00       0.39

pairwise.wilcox.test(data[data$region == "cpu",]$percent_area_adjusted,  
                     data[data$region == "cpu",]$sex_diet, paired = FALSE,
                     p.adj = "bonferroni", alternative = "two.sided", 
                     exact = FALSE)
#           femaleC/C femaleHF/C maleC/C
#femaleHF/C 1.00      -          -      
#maleC/C    0.03      0.03       -      
#maleHF/C   1.00      1.00       0.39 

# Two way testing: diet and region:
pairwise.wilcox.test(data$percent_area_adjusted,  
                     data$region_diet, paired = FALSE,
                     p.adj = "bonferroni", alternative = "two.sided", 
                     exact = FALSE)
#       cpuC/C cpuHF/C ctxC/C
#cpuHF/C 0.414  -       -     
#ctxC/C  1.000  0.099   -     
#ctxHF/C 0.413  1.000   0.135 

# Three way testing: diet, sex and region
pairwise.wilcox.test(data$percent_area_adjusted,  
                     data$sex_region_diet, paired = FALSE,
                     p.adj = "bonferroni", alternative = "two.sided", 
                     exact = FALSE)
# Nothing was significant likely due to correction of many groups

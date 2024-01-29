# One-way significance Testing -------------------------------------------------
# One way testing: diet, given region
leveneTest(percent_area_adjusted ~ diet, 
           data = data[data$region == "ctx",]) # p = 0.8335 equal variance
t.test(subset_list[["control_ctx"]], subset_list[["test_ctx"]], 
            alternative = "two.sided", exact = FALSE) # p-value = 0.07473 NS

leveneTest(percent_area_adjusted ~ diet, 
           data = data[data$region == "cpu",]) # p = 0.8754 equal variance
t.test(subset_list[["control_cpu"]], subset_list[["test_cpu"]], 
            alternative = "two.sided", exact = FALSE) # p-value = 0.226 NS

# One way testing: sex, given region
leveneTest(percent_area_adjusted ~ sex, 
           data = data[data$region == "ctx",]) # p = 0.8047 equal variance 
wilcox.test(subset_list[["female_ctx"]], subset_list[["male_ctx"]], 
            alternative = "two.sided", exact = FALSE) # p-value = 0.06555 NS

leveneTest(percent_area_adjusted ~ sex, 
           data = data[data$region == "cpu",]) # p = 0.9612 equal variance 
wilcox.test(subset_list[["female_cpu"]], subset_list[["male_cpu"]], 
            alternative = "two.sided", exact = FALSE) # p-value = 0.1282 NS

# One way testing: region
leveneTest(percent_area_adjusted ~ region, 
           data = data) # p = 0.7756 equal variance
wilcox.test(subset_list[["ctx"]], subset_list[["cpu"]], 
            alternative = "two.sided", exact = FALSE) # p-value = 1 NS


# Two-way significance testing--------------------------------------------------
# Two way testing: diet and sex, given region:
leveneTest(percent_area_adjusted ~ diet*sex, 
           data = data[data$region ==  "ctx",]) # p = 0.6507 equal variance
twoway_test_sex_ctx <- aov(percent_area_adjusted ~ diet+sex, 
                           data = data[data$region ==  "ctx",])
summary(twoway_test_sex_ctx)
#             Df Sum Sq Mean Sq F value Pr(>F)  
#diet         1 102.59  102.59   5.458 0.0443 *
#sex          1  86.15   86.15   4.583 0.0609 .
#Residuals    9 169.17   18.80  
twoway_test_sex_ctx_interaction <- aov(percent_area_adjusted ~ diet*sex, 
                                       data = data[data$region ==  "ctx",])
summary(twoway_test_sex_ctx_interaction)
#             Df Sum Sq Mean Sq F value Pr(>F)  
#diet         1 102.59  102.59   4.946 0.0568 .
#sex          1  86.15   86.15   4.154 0.0759 .
#diet:sex     1   3.24    3.24   0.156 0.7029  
#Residuals    8 165.92   20.74  

leveneTest(percent_area_adjusted ~ diet*sex, 
           data = data[data$region ==  "cpu",]) # p = 0.7177 equal variance
twoway_test_sex_cpu <- aov(percent_area_adjusted ~ diet+sex, 
                           data = data[data$region ==  "cpu",])
summary(twoway_test_sex_cpu)
#             Df Sum Sq Mean Sq F value Pr(>F)
#diet         1  69.89   69.89   2.031  0.188
#sex          1 109.97  109.97   3.196  0.107
#Residuals    9 309.68   34.41    
twoway_test_sex_cpu_interaction <- aov(percent_area_adjusted ~ diet*sex, 
                                       data = data[data$region ==  "cpu",])
summary(twoway_test_sex_cpu_interaction)
#             Df Sum Sq Mean Sq F value Pr(>F)
#diet         1  69.89   69.89   1.967  0.198
#sex          1 109.97  109.97   3.095  0.117
#diet:sex     1  25.44   25.44   0.716  0.422
#Residuals    8 284.24   35.53    

# Two way testing: diet and region:
leveneTest(percent_area_adjusted ~ diet*region, 
           data = data) # p = 0.8782 equal variance
twoway_test_region <- aov(percent_area_adjusted ~ diet+region, data = data)
summary(twoway_test_region)
#             Df Sum Sq Mean Sq F value Pr(>F)  
#diet         1  170.9  170.91   5.305 0.0316 *
#region       1    4.1    4.06   0.126 0.7263  
#Residuals   21  676.5   32.22     
twoway_test_region_interaction <- aov(percent_area_adjusted ~ diet*region, 
                                      data = data)
summary(twoway_test_region_interaction)
#             Df Sum Sq Mean Sq F value Pr(>F)  
#diet         1  170.9  170.91   5.064 0.0358 *
#region       1    4.1    4.06   0.120 0.7325  
#diet:region  1    1.6    1.56   0.046 0.8317  
#Residuals   20  675.0   33.75 

# Three way testing: diet, sex and region
leveneTest(percent_area_adjusted ~ diet*region*sex, 
           data = data) # p = 0.8471 equal variance
threeway_test <- aov(percent_area_adjusted ~ diet+region+sex, data = data)
summary(threeway_test)
#             Df Sum Sq Mean Sq F value Pr(>F)   
#diet         1  170.9  170.91   7.105 0.0149 * 
#region       1    4.1    4.06   0.169 0.6857   
#sex          1  195.4  195.39   8.122 0.0099 **
#Residuals   20  481.1   24.06 
threeway_test_interaction <- aov(percent_area_adjusted ~ diet*region*sex, 
                                 data = data)
summary(threeway_test_interaction)
#                 Df Sum Sq Mean Sq F value Pr(>F)  
#diet             1  170.9  170.91   6.075 0.0254 *
#region           1    4.1    4.06   0.144 0.7092  
#sex              1  195.4  195.39   6.945 0.0180 *
#diet:region      1    1.6    1.56   0.056 0.8166  
#diet:sex         1   23.4   23.42   0.832 0.3751  
#region:sex       1    0.7    0.73   0.026 0.8744  
#diet:region:sex  1    5.3    5.26   0.187 0.6713  
#Residuals       16  450.2   28.14 

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
#                                K   AICc Delta_AICc AICcWt Cum.Wt     LL
#twoway_test_sex_ctx             4  79.52       0.00   0.93   0.93 -32.90
#twoway_test_sex_ctx_interaction 5  85.57       6.05   0.05   0.97 -32.79
#twoway_test_sex_cpu             4  86.78       7.26   0.02   1.00 -36.53
#twoway_test_sex_cpu_interaction 5  92.03      12.51   0.00   1.00 -36.02
#threeway_test                   5 153.40      73.88   0.00   1.00 -70.03
#twoway_test_region              4 158.35      78.83   0.00   1.00 -74.12
#twoway_test_region_interaction  5 161.52      82.00   0.00   1.00 -74.09
#threeway_test_interaction       9 169.32      89.80   0.00   1.00 -69.23


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
pairwise.t.test(data[data$region == "ctx",]$percent_area_adjusted,  
                     data[data$region == "ctx",]$sex_diet, paired = FALSE,
                     p.adj = "bonferroni",
                     alternative = "two.sided", 
                     exact = FALSE)
#           femaleC/C femaleHF/C maleC/C
#femaleHF/C 1.00      -          -      
#maleC/C    0.74      0.10       -      
#maleHF/C   1.00      1.00       0.61 

pairwise.t.test(data[data$region == "cpu",]$percent_area_adjusted,  
                     data[data$region == "cpu",]$sex_diet, paired = FALSE,
                     p.adj = "bonferroni", alternative = "two.sided", 
                     exact = FALSE)
#           femaleC/C femaleHF/C maleC/C
#femaleHF/C 1.00      -          -      
#maleC/C    0.62      0.33       -      
#maleHF/C   1.00      1.00       0.90   

# Two way testing: diet and region:
pairwise.t.test(data$percent_area_adjusted,  
                     data$region_diet, paired = FALSE,
                     p.adj = "bonferroni", alternative = "two.sided", 
                     exact = FALSE)
#        cpuC/C cpuHF/C ctxC/C
#cpuHF/C 0.99   -       -     
#ctxC/C  1.00   0.49    -     
#ctxHF/C 1.00   1.00    0.58  

# Three way testing: diet, sex and region
pairwise.wilcox.test(data$percent_area_adjusted,  
                     data$sex_region_diet, paired = FALSE,
                     p.adj = "bonferroni", alternative = "two.sided", 
                     exact = FALSE)
#femalecpuC/C femalecpuHF/C femalectxC/C femalectxHF/C malecpuC/C
#femalecpuHF/C 1            -             -            -             -         
#femalectxC/C  1            1             -            -             -         
#femalectxHF/C 1            1             1            -             -         
#malecpuC/C    1            1             1            1             -         
#malecpuHF/C   1            1             1            1             1         
#malectxC/C    1            1             1            1             1         
#malectxHF/C   1            1             1            1             1         

#malecpuHF/C malectxC/C
#femalecpuHF/C -           -         
#femalectxC/C  -           -         
#femalectxHF/C -           -         
#malecpuC/C    -           -         
#malecpuHF/C   -           -         
#malectxC/C    1           -         
#malectxHF/C   1           1  
# Nothing was significant likely due to correction of many groups

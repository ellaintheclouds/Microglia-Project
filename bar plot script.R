# One-way comparison -----------------------------------------------------------
stats_df$diet <- NA
stats_df[3:6, "diet"] <- c("C/C", "HF/C", "C/C", "HF/C")
stats_df$sex <- NA
stats_df[7:10, "sex"] <- c("male", "female", "male", "female")

grob1a <- (# Diet (cortex)
  stats_df[3:4, ] |> 
    ggplot(aes(x = diet, y = mean)) +
    geom_bar(stat = "identity", width = 0.5, fill = "cornflowerblue") + 
    geom_point(data = data[data$region == "ctx",], 
               aes(x = diet, y = percent_area_adjusted)) +
    geom_errorbar(aes(ymin = mean-se, ymax = mean + se, width = 0.1)) +
    geom_signif(comparisons = list(c("C/C", "HF/C")), 
                annotation = "*") + 
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    ggtitle("Cortex") +
    xlab("Paternal Diet") + ylab("Mean Microglia Coverage (%)") +
    scale_x_discrete(labels=c("Control", "High-Fat")) + 
    theme_bw()
)
ggsave(grob1a, filename = "Output/Graphs/oneway comparison/diet (cortex).png",
         width = 5, height = 5)

grob1b <- (# Diet (striatum)
  stats_df[5:6, ] |> 
    ggplot(aes(x = diet, y = mean)) +
    geom_bar(stat = "identity", width = 0.5, fill = "cornflowerblue") +
    geom_point(data = data[data$region == "cpu",], 
               aes(x = diet, y = percent_area_adjusted)) +
    geom_errorbar(aes(ymin = mean-se, ymax = mean + se, width = 0.1)) +
    scale_y_continuous(#breaks = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24),
      expand = expansion(mult = c(0, 0.05))) +
    ggtitle("Striatum") +
    xlab("Paternal Diet") + ylab("Mean Microglia Coverage (%)") +
    scale_x_discrete(labels=c("Control", "High-Fat")) + 
    theme_bw()
)
ggsave(grob1b, filename = "Output/Graphs/oneway comparison/diet (striatum).png",
         width = 5, height = 5)

grob2a <- (# Sex (cortex)
  stats_df[7:8, ] |> 
    ggplot(aes(x = sex, y = mean)) +
    geom_bar(stat = "identity", width = 0.5, fill = "cornflowerblue") +
    geom_point(data = data[data$region == "ctx",], 
               aes(x = sex, y = percent_area_adjusted)) +
    geom_errorbar(aes(ymin = mean-se, ymax = mean + se, width = 0.1)) +
    geom_signif(comparisons = list(c("male", "female")), 
                annotation = "*") + 
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    ggtitle("Cortex") +
    xlab("Sex") + ylab("Mean Microglia Coverage (%)") +
    scale_x_discrete(labels=c("Female", "Male")) + 
    theme_bw()
)
ggsave(grob2a, filename = "Output/Graphs/oneway comparison/sex (cortex).png",
         width = 5, height = 5)

grob2b <- (# Sex (striatum)
  stats_df[9:10, ] |> 
    ggplot(aes(x = sex, y = mean)) +
    geom_bar(stat = "identity", width = 0.5, fill = "cornflowerblue") +
    geom_point(data = data[data$region == "cpu",], 
               aes(x = sex, y = percent_area_adjusted)) +
    geom_errorbar(aes(ymin = mean-se, ymax = mean + se, width = 0.1)) +
    geom_signif(comparisons = list(c("male", "female")), 
                annotation = "*") + 
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    ggtitle("Striatum") +
    xlab("Sex") + ylab("Mean Microglia Coverage (%)") +
    scale_x_discrete(labels=c("Female", "Male")) + 
    theme_bw()
)
ggsave(grob2b, filename = "Output/Graphs/oneway comparison/sex (striatum).png",
         width = 5, height = 5)

region_plot <- (# Region
  stats_df[1:2, ] |> 
    ggplot(aes(x = test, y = mean)) +
    geom_bar(stat = "identity", width = 0.5, fill = "cornflowerblue") +
    geom_point(data = data, aes(x = region, y = percent_area_adjusted)) +
    geom_errorbar(aes(ymin = mean-se, ymax = mean + se, width = 0.1)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    xlab("Region") + ylab("Mean Microglia Coverage (%)") +
    scale_x_discrete(labels=c("Striatium", "Cortex")) + 
    theme_bw()
)
ggsave(region_plot, filename = "Output/Graphs/oneway comparison/region.png",
         width = 5, height = 5)  


# Two-way comparison -----------------------------------------------------------
stats_df_twoway <- stats_df[11:18, ]
stats_df_twoway$sex <- c("male", "female", "male", "female", 
                         "male", "female", "male", "female")
stats_df_twoway$diet <- c("C/C", "C/C", "HF/C", "HF/C",
                          "C/C", "C/C", "HF/C", "HF/C")

grob3a <- (# Diet and sex (cortex) grouped by diet
  stats_df_twoway[1:4, ] |> 
    ggplot(aes(x = diet, y = mean, fill = sex)) +
    geom_bar(stat = "identity", position=position_dodge(), width = 0.5) +
    geom_point(data = data[data$region == "ctx",], 
               aes(x = diet, y = percent_area_adjusted), 
               position=position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = mean-se, ymax = mean + se, width = 0.1), 
                  position=position_dodge(.5)) +
    guides(fill=guide_legend(title="Offspring Sex")) +
    scale_fill_manual(values = c("#61B499", "#8E61B4")) + 
    geom_signif(stat = "identity", aes(x = 0.8, xend = 1.2, y = 10, yend = 10, 
                                       annotation = "*")) + 
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    ggtitle("Cortex") +
    xlab("Paternal Diet") + ylab("Mean Microglia Coverage (%)") +
    scale_x_discrete(labels=c("Control", "High-Fat")) + 
    theme_bw() + 
    theme(legend.position="none")
  
)
ggsave(grob3a, filename = 
           "Output/Graphs/twoway comparison/diet and sex (cortex) grouped by diet.png",
         width = 5, height = 5)                         

grob3b <- (# Diet and sex (cortex) grouped by sex
  stats_df_twoway[1:4, ] |> 
    ggplot(aes(x = sex, y = mean, fill = diet)) +
    geom_bar(stat = "identity", position=position_dodge(), width = 0.5) +
    geom_point(data = data[data$region == "ctx",], 
               aes(x = sex, y = percent_area_adjusted), 
               position=position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = mean-se, ymax = mean + se, width = 0.1), 
                  position=position_dodge(.5)) +
    guides(fill=guide_legend(title="Paternal Diet")) +
    scale_fill_manual(values = c("#6185B4", "#B461AE")) + 
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    ggtitle("Cortex") +
    xlab("Offspring Sex") + ylab("Mean Microglia Coverage (%)") +
    scale_x_discrete(labels=c("Female", "Male")) + 
    theme_bw() + 
    theme(legend.position="none")
)
ggsave(grob3b, filename = 
           "Output/Graphs/twoway comparison/diet and sex (cortex) grouped by sex.png",          
         width = 5, height = 5)                                                     

grob3c <- (# Diet and sex (striatum) grouped by diet
  stats_df_twoway[5:8, ] |> 
    ggplot(aes(x = diet, y = mean, fill = sex)) +
    geom_bar(stat = "identity", position=position_dodge(), width = 0.5) +
    geom_point(data = data[data$region == "cpu",], 
               aes(x = diet, y = percent_area_adjusted), 
               position=position_dodge(width = 0.5)) +    
    geom_errorbar(aes(ymin = mean-se, ymax = mean + se, width = 0.1), 
                  position=position_dodge(.5)) +
    guides(fill=guide_legend(title="Offspring Sex")) +
    scale_fill_manual(values = c("#61B499", "#8E61B4")) + 
    geom_signif(stat = "identity", aes(x = 0.8, xend = 1.2, y = 14, yend = 14, 
                                       annotation = "*")) + 
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    ggtitle("Striatum") +
    xlab("Paternal Diet") + ylab("Mean Microglia Coverage (%)") +
    scale_x_discrete(labels=c("Control", "High-Fat")) + 
    theme_bw() + 
    theme(legend.position="bottom")
)
ggsave(grob3c, filename = 
           "Output/Graphs/twoway comparison/dot diet and sex (striatum) grouped by diet .png",
         width = 5, height = 5.5)

grob3d <- (# Diet and sex (striatum) grouped by sex
  stats_df_twoway[5:8, ] |> 
    ggplot(aes(x = sex, y = mean, fill = diet)) +
    geom_bar(stat = "identity", position=position_dodge(), width = 0.5) +
    geom_point(data = data[data$region == "cpu",], 
               aes(x = sex, y = percent_area_adjusted), 
               position=position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = mean-se, ymax = mean + se, width = 0.1), 
                  position=position_dodge(.5)) +
    guides(fill=guide_legend(title="Paternal Diet")) +
    scale_fill_manual(values = c("#6185B4", "#B461AE"), 
                      labels = c ("Control", "High-Fat")) + 
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    ggtitle("Striatum") +
    xlab("Offspring Sex") + ylab("Mean Microglia Coverage (%)") +
    scale_x_discrete(labels=c("Female", "Male")) + 
    theme_bw() + 
    theme(legend.position="bottom")
)
ggsave(grob3d, filename = 
           "Output/Graphs/twoway comparison/diet and sex (striatum) grouped by sex.png",
         width = 5, height = 5.5)

stats_df_twoway_ <- stats_df[3:6, ]
stats_df_twoway_$region <- c("ctx", "ctx", "cpu", "cpu")
stats_df_twoway_$diet <- c("C/C", "HF/C", "C/C", "HF/C")

grob4a <- (# Diet and region grouped by diet
  stats_df_twoway_ |> 
    ggplot(aes(x = diet, y = mean, fill =region)) + 
    geom_bar(stat = "identity", position=position_dodge(), width = 0.5) +
    geom_point(data = data, aes(x = diet, y = percent_area_adjusted), 
               position=position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = mean-se, ymax = mean + se, width = 0.1), 
                  position=position_dodge(.5)) +
    guides(fill=guide_legend(title="Region")) +
    scale_fill_manual(values = c("#B4B361", "#B46167"), 
                      labels = c("Striatum", "Cortex")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    xlab("Diet (Paternal/Offspring)") + ylab("Mean Microglia Coverage (%)") +
    scale_x_discrete(labels=c("Control", "High-Fat")) + 
    theme_bw() + 
    theme(legend.position="bottom")
)
ggsave(grob4a, filename = 
           "Output/Graphs/twoway comparison/diet and region grouped by diet .png",
         width = 5, height = 5.5)

grob4b <- (# Diet and region grouped by region
  stats_df_twoway_ |> 
    ggplot(aes(x = region, y = mean, fill =diet)) + 
    geom_bar(stat = "identity", position=position_dodge(), width = 0.5) +
    geom_point(data = data, aes(x = region, y = percent_area_adjusted), 
               position=position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = mean-se, ymax = mean + se, width = 0.1), 
                  position=position_dodge(.5)) +
    guides(fill=guide_legend(title="Diet")) +
    scale_fill_manual(values = c("#6185B4", "#B461AE"), 
                      labels = c("Control", "High-Fat")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    xlab("Region") + ylab("Mean Microglia Coverage (%)") +
    scale_x_discrete(labels=c("Striatum", "Cortex")) + 
    theme_bw() + 
    theme(legend.position="bottom")
)
ggsave(grob4b, filename = 
           "Output/Graphs/twoway comparison/diet and region grouped by region.png",
         width = 5, height = 5.5)

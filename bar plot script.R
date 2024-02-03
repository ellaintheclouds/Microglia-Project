# One-way comparison -----------------------------------------------------------
stats_df$diet <- NA
stats_df[3:6, "diet"] <- c("C/C", "HF/C", "C/C", "HF/C")
stats_df$region <- NA
stats_df[3:10, "region"] <- c("ctx", "ctx", "cpu", "cpu", "ctx", "ctx", "cpu", "cpu")
stats_df$sex <- NA
stats_df[7:10, "sex"] <- c("male", "female", "male", "female")


region.labs <- c("Striatum", "Cortex")
names(region.labs) <- c("cpu", "ctx")

annotation_df1 <- data.frame(
  region = c("cpu", "ctx"),
  start = c("C/C", "C/C"),
  end = c("HF/C", "HF/C"),
  y = c(22, 20),
  label = c("p = 0.23", "p = 0.075")
)

diet_facet <-
  stats_df[3:6, ] |> 
  ggplot(aes(x = diet, y = mean)) +
  geom_bar(stat = "identity", width = 0.5, fill = "deepskyblue3") + 
  geom_point(data = data[data$region == "ctx",], 
             aes(x = diet, y = percent_area_adjusted)) +
  geom_point(data = data[data$region == "cpu",], 
             aes(x = diet, y = percent_area_adjusted)) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean + se, width = 0.1)) +
  geom_signif(data = annotation_df1,
    aes(xmin = start, xmax = end, annotations = label, y_position = y),
    textsize = 3, vjust = -0.2,
    manual = TRUE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  ggtitle("Effect of Paternal Diet on Iba1 Coverage") +
  xlab("Paternal Diet") + ylab("Mean Iba1 Coverage (%)") +
  scale_x_discrete(labels=c("Control", "High-Fat")) + 
  theme_bw() + 
  theme(plot.title = element_text(size=12, face="bold", 
                                  margin = margin(10, 0, 10, 0))) + 
  facet_wrap(~ region, labeller = labeller(region = region.labs)) 
ggsave(diet_facet, filename = "Output/Graphs/oneway comparison/diet facet.png",
       width = 6, height = 4)

annotation_df2 <- data.frame(
  region = c("cpu", "ctx"),
  start = c("female", "female"),
  end = c("male", "male"),
  y = c(22, 20),
  label = c("p = 0.13", "p = 0.066")
)

sex_facet <-
  stats_df[7:10, ] |> 
  ggplot(aes(x = sex, y = mean)) +
  geom_bar(stat = "identity", width = 0.5, fill = "deepskyblue3") + 
  geom_point(data = data[data$region == "ctx",], 
             aes(x = sex, y = percent_area_adjusted)) +
  geom_point(data = data[data$region == "cpu",], 
             aes(x = sex, y = percent_area_adjusted)) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean + se, width = 0.1)) +
  geom_signif(data = annotation_df2,
              aes(xmin = start, xmax = end, annotations = label, y_position = y),
              textsize = 3, vjust = -0.2,
              manual = TRUE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  ggtitle("Effect of Sex on Iba1 Coverage") +
  xlab("Offspring Sex") + ylab("Mean Iba1 Coverage (%)") +
  scale_x_discrete(labels=c("Female", "Male")) + 
  theme_bw() + 
  theme(plot.title = element_text(size=12, face="bold", 
                                  margin = margin(10, 0, 10, 0))) + 
  facet_wrap(~ region, labeller = labeller(region = region.labs)) 
ggsave(diet_facet, filename = "Output/Graphs/oneway comparison/sex facet.png",
       width = 6, height = 4)

region_plot <- # Region
  stats_df[1:2, ] |> 
  ggplot(aes(x = test, y = mean)) +
  geom_bar(stat = "identity", width = 0.5, fill = "deepskyblue3") +
  geom_point(data = data, aes(x = region, y = percent_area_adjusted)) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean + se, width = 0.1)) +
  geom_signif(comparisons = list(c("ctx", "cpu")), 
              annotation = "p = 1") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  ggtitle("Iba1 Coverage in Cortex verses Striatum") +
  xlab("Region") + ylab("Mean Iba1 Coverage (%)") +
  scale_x_discrete(labels=c("Striatium", "Cortex")) + 
  theme_bw() + 
  theme(plot.title = element_text(size=12, face="bold", 
                                               margin = margin(10, 0, 10, 0)))
ggsave(region_plot, filename = "Output/Graphs/oneway comparison/region.png",
       width = 4, height = 4)


# Two-way comparison -----------------------------------------------------------
stats_df_twoway <- stats_df[11:18, ]
stats_df_twoway$sex <- c("male", "female", "male", "female", 
                         "male", "female", "male", "female")
stats_df_twoway$diet <- c("C/C", "C/C", "HF/C", "HF/C",
                          "C/C", "C/C", "HF/C", "HF/C")
stats_df_twoway$region <- c("ctx", "ctx", "ctx", "ctx", "cpu", "cpu", "cpu", "cpu")

annotation_df3 <- data.frame(region = c("cpu", "cpu", "ctx", "ctx"),
                             y_position = c(19, 22, 14, 20), 
                             xmin = c(0.8, 1.8, 0.8, 1.8), 
                             xmax = c(1.2, 2.2, 1.2, 2.2),
                             label = c("p = 0.62", "p = 1", "p = 0.74", "p = 1")
                             )

sex_diet_facet <-
  stats_df_twoway[1:8, ] |> 
  ggplot(aes(x = diet, y = mean, fill = sex)) +
  geom_bar(stat = "identity", position=position_dodge(), width = 0.5) + 
  geom_point(data = data[data$region == "ctx",], 
             aes(x = diet, y = percent_area_adjusted), 
             position=position_dodge(width = 0.5)) +
  geom_point(data = data[data$region == "cpu",], 
             aes(x = diet, y = percent_area_adjusted), 
             position=position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean + se, width = 0.1), 
                position=position_dodge(.5)) +
  geom_signif(data = annotation_df3, inherit.aes=FALSE,
              aes(xmin = xmin, xmax = xmax, annotations = label, y_position = y_position),
              textsize = 3, vjust = -0.2,
              manual = TRUE)+
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  guides(fill=guide_legend(title="Offspring Sex")) +
  scale_fill_manual(values = c("#61B499", "#8E61B4"), labels = c("Female", "Male")) + 
  ggtitle("Effect of Paternal Diet on Iba1 Coverage (Grouped by Sex)") +
  xlab("Paternal Diet") + ylab("Mean Iba1 Coverage (%)") +
  scale_x_discrete(labels=c("Control", "High-Fat")) + 
  theme_bw() + 
  theme(plot.title = element_text(size=12, face="bold", 
                                  margin = margin(10, 0, 10, 0))) + 
  facet_wrap(~ region, labeller = labeller(region = region.labs)) 
ggsave(sex_diet_facet, filename = "Output/Graphs/twoway comparison/sex diet facet.png",
       width = 6, height = 4)

annotation_df4 <- data.frame(region = c("cpu", "cpu", "ctx", "ctx"),
                             y_position = c(22, 17, 20, 13), 
                             xmin = c(0.8, 1.8, 0.8, 1.8), 
                             xmax = c(1.2, 2.2, 1.2, 2.2),
                             label = c("p = 1", "p = 0.9", "p = 1", "p = 0.61")
)

diet_sex_facet <-
  stats_df_twoway[1:8, ] |> 
  ggplot(aes(x = sex, y = mean, fill = diet)) +
  geom_bar(stat = "identity", position=position_dodge(), width = 0.5) + 
  geom_point(data = data[data$region == "ctx",], 
             aes(x = sex, y = percent_area_adjusted), 
             position=position_dodge(width = 0.5)) +
  geom_point(data = data[data$region == "cpu",], 
             aes(x = sex, y = percent_area_adjusted), 
             position=position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean + se, width = 0.1), 
                position=position_dodge(.5)) +
    geom_signif(data = annotation_df4, inherit.aes=FALSE,
                aes(xmin = xmin, xmax = xmax, annotations = label, y_position = y_position),
                textsize = 3, vjust = -0.2,
                manual = TRUE)+
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  guides(fill=guide_legend(title="Paternal Diet")) +
  scale_fill_manual(values = c("#6185B4", "#B461AE"), labels = c("Control", "High-Fat")) + 
  ggtitle("Effect of Sex on Iba1 Coverage (Grouped by Paternal Diet)") +
  xlab("Offspring Sex") + ylab("Mean Iba1 Coverage (%)") +
  scale_x_discrete(labels=c("Female", "Male")) + 
  theme_bw() + 
  theme(plot.title = element_text(size=12, face="bold", 
                                  margin = margin(10, 0, 10, 0))) + 
  facet_wrap(~ region, labeller = labeller(region = region.labs)) 
ggsave(diet_sex_facet, filename = "Output/Graphs/twoway comparison/diet sex facet.png",
      width = 6, height = 4)

stats_df_twoway_ <- stats_df[3:6, ]
stats_df_twoway_$region <- c("ctx", "ctx", "cpu", "cpu")
stats_df_twoway_$diet <- c("C/C", "HF/C", "C/C", "HF/C")

grob4a <- # Diet and region grouped by diet
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
    ggtitle("Effect of Region on Iba1 Coverage 
            (Grouped by Paternal Diet)") +
    xlab("Diet (Paternal/Offspring)") + ylab("Mean Iba1 Coverage (%)") +
    scale_x_discrete(labels=c("Control", "High-Fat")) + 
    theme_bw() + 
    theme(plot.title = element_text(size=12, face="bold", 
                                  margin = margin(10, 0, 10, 0)))
ggsave(grob4a, filename = 
           "Output/Graphs/twoway comparison/diet region facet.png",
         width = 4.4, height = 4)

grob4b <- # Diet and region grouped by region
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
    ggtitle("Effect of Paternal Diet on Iba1 Coverage 
            (Grouped by Sex)") +
    xlab("Region") + ylab("Mean Iba1 Coverage (%)") +
    scale_x_discrete(labels=c("Striatum", "Cortex")) + 
    theme_bw() + 
    theme(plot.title = element_text(size=12, face="bold", 
                                margin = margin(10, 0, 10, 0)))
ggsave(grob4b, filename = 
           "Output/Graphs/twoway comparison/region diet facet.png",
         width = 4.4, height = 4)

# Three-way comparison ---------------------------------------------------------
stats_df_threeway <- stats_df[11:18,]
stats_df_threeway$diet <- c("C/C", "C/C", "HF/C", "HF/C", 
                            "C/C", "C/C", "HF/C", "HF/C")
stats_df_threeway$sex <- c("male", "female", "male", "female", 
                            "male", "female", "male", "female")
stats_df_threeway$region <- c("ctx", "ctx", "ctx", "ctx", 
                              "cpu", "cpu", "cpu", "cpu")
stats_df_threeway$sexdiet <- paste0(stats_df_threeway$sex, stats_df_threeway$diet)

data$sex_diet <- paste0(data$sex, data$diet)

annotation_df5 <- data.frame(y_position = c(19, 22, 3, 17), 
                             xmin = c(0.8, 1.8, 2.8, 3.8), 
                             xmax = c(1.2, 2.2, 3.2, 4.2),
                             label = c("p = 1", "p = 1 ", " p = 1", " p = 1 ")
                             )

grob5 <- # Diet, sex and region
  stats_df_threeway |> 
    ggplot(aes(x = sexdiet, y = mean, fill =region)) + 
    geom_bar(stat = "identity", position=position_dodge(), width = 0.5) +
    geom_point(data = data, aes(x = sex_diet , y = percent_area_adjusted), 
               position=position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = mean-se, ymax = mean + se, width = 0.1), 
                  position=position_dodge(.5)) +
    geom_signif(inherit.aes=FALSE, data = annotation_df5,
                aes(y_position = y_position, xmin = xmin, xmax = xmax, annotations = label),
                textsize = 3, vjust = -0.2, manual = TRUE)+
    guides(fill=guide_legend(title="Region")) +
    scale_fill_manual(values = c("#B4B361", "#B46167"), 
                      labels = c("Striatum", "Cortex")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.07))) +
    ggtitle("Effect of Region on Iba1 Coverage (Grouped by Sex and Paternal Diet)") +
    xlab("Offspring Sex and Paternal Diet") + ylab("Mean Iba1 Coverage (%)") +
    scale_x_discrete(labels=c("Female Control", "Female High-Fat", 
                             "Male Control", "Male High-Fat")) + 
    theme_bw() + 
    theme(plot.title = element_text(size=12, face="bold", 
                                  margin = margin(10, 0, 10, 0)))
ggsave(grob5, filename = 
         "Output/Graphs/threeway comparison/diet, sex and region.png",
       width = 7, height = 4)

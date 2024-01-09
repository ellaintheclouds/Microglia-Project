grid.arrange(grob1a, grob1b, ncol = 2, nrow = 1) |>
ggsave(filename =                                                               
      "Output/Graphs/oneway comparison/diet.png", width = 10, height = 5)

grid.arrange(grob2a, grob2b, ncol = 2, nrow = 1) |>
  ggsave(filename =                                                             
           "Output/Graphs/oneway comparison/sex.png", width = 10, height = 5)

grid.arrange(grob3a, grob3b, grob3c, grob3d, ncol = 2, nrow = 2) |>
  ggsave(filename =                                                             
           "Output/Graphs/twoway comparison/diet and sex.png", width = 10, height = 10.5)

grid.arrange(grob4a, grob4b, ncol = 2, nrow = 1) |>
  ggsave(filename =                                                             
           "Output/Graphs/twoway comparison/diet and region.png", width = 10, 
         height = 5.5)

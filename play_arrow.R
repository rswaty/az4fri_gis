

## try arrow chart with reference points


library(tidyverse)

play_arrow <- read_csv("play_arrow.csv")



arrow_plot <- play_arrow |> 
  ggplot(aes(
    x = scl2020, xend = scl2022, 
    y = reorder(class, desc(class)),  yend = class,
    color = sign_change)) +
  geom_segment(
    arrow = arrow(angle = 30, length = unit(0.5, 'cm')),
    size = 3) +
  geom_point(aes(x = ref_percent, y = class), shape = 19, size = 6, color = 'black') +
  labs(
    x = 'Percent Change', 
    y = element_blank(),
    title = 'Changes in Succession Class Percents, 2020 to 2022',
    subtitle = 'Arrow beginning = % in 2020, End = % in 2022; Dot = Reference %'
  ) +
  scale_color_manual(
    values = c("#fcba03", "#10692c")) +
  theme_bw(base_size = 12) + 
  theme(legend.position = "none")


arrow_plot

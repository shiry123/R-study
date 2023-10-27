library(patchwork)
p1 <- ggplot(mtcars) + 
  geom_point(aes(mpg, disp), colour = "#7fc97f") + 
  ggtitle('Plot 1')

p2 <- ggplot(mtcars) + 
  geom_boxplot(aes(gear, disp, fill = factor(gear)), 
               show.legend = FALSE) + 
  ggtitle('Plot 2')

p3 <- ggplot(mtcars) + 
  geom_point(aes(hp, wt, colour = mpg)) + 
  scale_colour_gradientn(
    colours = c("#66c2a5", "#fc8d62", "#8da0cb")) +
  ggtitle('Plot 3')

p4 <- ggplot(mtcars) + 
  geom_bar(aes(gear, fill = factor(gear)), 
           show.legend = FALSE) + 
  facet_wrap(~cyl) + 
  ggtitle('Plot 4')
p1+p2
p1/p2
p1 / (p2 | p3)
p1 + gridExtra::tableGrob(mtcars[1:10, c('mpg', 'disp')])
plot(mtcars$mpg, mtcars$disp, main = 'Plot 2', 
           col = if_else(mtcars$disp > 250, "red", "green"))
p1 + inset_element(p2, left = 0.6, bottom = 0.6, right = 1, top = 1)
p1 + p2 + p3 + p4 +
  plot_layout(guides = 'collect')
((p2 / p3 + plot_layout(guides = 'keep')) | p1) + plot_layout(guides = 'collect')
# auto：如果嵌套的上层尝试收集图例，则也会进行收集，否则，放置在图形边上
# collect：会将制定嵌套级别的图例收集起来，并删除重复的图例。还可以是 keep
# keep：将图例放置在对应的图形边上
patchwork <- (p1 + p2) / p3
patchwork + plot_annotation(
  title = 'The surprising truth about mtcars',
  subtitle = 'These 3 plots will reveal yet-untold secrets about our beloved data-set',
  caption = 'Disclaimer: None of these plots are insightful'
)
patchwork + plot_annotation(tag_levels = 'A')
# 1：阿拉伯数字
# a：小写字母
# A：大写字母
# i：小写罗马数字
# I：大写罗马数字

library(cowplot)
ggplot(iris, aes(Sepal.Length, fill = Species)) + 
  geom_density(alpha = 0.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_minimal_hgrid(font_size = 12)
p1 <- ggplot(mtcars, aes(disp, mpg)) + 
  geom_point(colour = 'red')
p1+theme_minimal_grid(font_size = 12)

p2 <- ggplot(mtcars, aes(disp, hp)) + 
  geom_point(colour = 'blue')
plot_grid(p1, p2, ncol = 1, align = "v")

p1 <- ggplot(mtcars, aes(disp, mpg)) + 
  geom_point() +
  theme_minimal_grid(14) + 
  panel_border(color = "black")

p2 <- ggplot(mtcars, aes(factor(vs), colour = factor(vs))) + 
  geom_bar() + 
  facet_wrap(~am) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal_hgrid(12) +
  panel_border(color = "black") +
  theme(strip.background = element_rect(fill = "gray80"))
plot_grid(p1, p2, align = "h", axis = "b", rel_widths = c(1, 1.3))
#组合图形
city_mpg <- mpg %>%
  mutate(class = fct_lump(class, 4, other_level = "other")) %>%
  group_by(class) %>%
  summarize(
    mean_mpg = mean(cty),
    count = n()
  ) %>% mutate(
    class = fct_reorder(class, count)
  )
city_mpg <- city_mpg %>%
  mutate(class = fct_reorder(class, -mean_mpg))

p1 <- ggplot(city_mpg, aes(class, count)) +
  geom_col(fill = "#6297E770") + 
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    position = "right"
  ) +
  theme_minimal_hgrid(11, rel_small = 1) +
  theme(
    panel.grid.major = element_line(color = "#6297E770"),
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.length = grid::unit(0, "pt"),
    axis.text.y = element_text(color = "#6297E7"),
    axis.title.y = element_text(color = "#6297E7")
  )
p1
p2 <- ggplot(city_mpg, aes(class, mean_mpg)) + 
  geom_point(size = 3, color = "#D5442D") + 
  scale_y_continuous(limits = c(10, 21)) +
  theme_half_open(11, rel_small = 1) +
  theme(
    axis.ticks.y = element_line(color = "#BB2D05"),
    axis.text.y = element_text(color = "#BB2D05"),
    axis.title.y = element_text(color = "#BB2D05"),
    axis.line.y = element_line(color = "#BB2D05")
  )
p2
aligned_plots <- align_plots(p1, p2, align="hv", axis="tblr")
ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])


plot.mpg <- ggplot(mpg, aes(x = cty, y = hwy, colour = factor(cyl))) + 
  geom_point(size=2.5)+
  theme_cowplot(font_size = 12)
plot.mpg
plot.mpg + background_grid(major = "xy", minor = "none")
# use save_plot() instead of ggsave() when using cowplot
save_plot("mpg.png", plot.mpg,
          base_aspect_ratio = 1.3 # make room for figure legend
)
plot.diamonds <- ggplot(diamonds, aes(clarity, fill = cut)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle=70, vjust=0.5))
plot.diamonds
plot_grid(plot.mpg, plot.diamonds, labels = c("A", "B"))

# plot.mpg and plot.diamonds were defined earlier
library(viridis)
ggdraw() +
  draw_plot(plot.diamonds + theme(legend.justification = "bottom"), 0, 0, 1, 1) +
  draw_plot(plot.mpg + scale_color_viridis(discrete = TRUE) + 
              theme(legend.justification = "top"), 0.5, 0.52, 0.5, 0.4) +
  draw_plot_label(c("A", "B"), c(0, 0.5), c(1, 0.92), size = 15)
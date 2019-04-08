library(ggplot2)

d0 <- data.frame(
  x = rep(0, 10), 
  x1 = seq(from = 1, to = 10, by = 1), 
  t = c(rep(1, 5), rep(0, 5))
)

d1 <- data.frame(
  x = rep(1, 10), 
  x1 = c(seq(from = 1, to = 4, by = 0.8), 4.4, 5.4, 6.4, 7.7, 8.8, 9.9)
)

g <- ggplot() +
  geom_point(data = d0, aes(x = 'd0', color = factor(t), y = x1)) +
  geom_point(data = d1, aes(x = 'd1', y = x1)) +
  coord_flip() +
  geom_segment(aes(x = 'd1', xend = 'd0', y = 1, yend = 1),
               arrow = arrow(length = unit(0.2, "cm"))) +
  geom_segment(aes(x = 'd1', xend = 'd0', y = 1.8, yend = 2),
               arrow = arrow(length = unit(0.2, "cm"))) +
  geom_segment(aes(x = 'd1', xend = 'd0', y = 2.6, yend = 3),
               arrow = arrow(length = unit(0.2, "cm"))) +
  geom_segment(aes(x = 'd1', xend = 'd0', y = 3.4, yend = 3),
               arrow = arrow(length = unit(0.2, "cm"))) +
  geom_segment(aes(x = 'd1', xend = 'd0', y = 4.4, yend = 4),
               arrow = arrow(length = unit(0.2, "cm"))) +
  geom_segment(aes(x = 'd1', xend = 'd0', y = 5.4, yend = 5),
               arrow = arrow(length = unit(0.2, "cm"))) +
  geom_segment(aes(x = 'd1', xend = 'd0', y = 6.4, yend = 6),
               arrow = arrow(length = unit(0.2, "cm"))) +
  geom_segment(aes(x = 'd1', xend = 'd0', y = 7.7, yend = 8),
               arrow = arrow(length = unit(0.2, "cm"))) + 
  geom_segment(aes(x = 'd1', xend = 'd0', y = 8.8, yend = 9),
               arrow = arrow(length = unit(0.2, "cm"))) +
  geom_segment(aes(x = 'd1', xend = 'd0', y = 9.9, yend = 10),
               arrow = arrow(length = unit(0.2, "cm")))

g + ylab('') + xlab('') + 
  labs(col = "") + 
  ggsave('images/dots_example.png')     

set.seed(123)
sample(1:10)

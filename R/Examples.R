las_file = '/Users/steven/GitHub/LASinfections/examples/ex9_1046102267.las'
newlas = las(las_file)
View(newlas$las_version)
newlas$well
View(newlas$curves)
head(newlas$log_data)

plot(newlas)

tmp = select(newlas$log_data, GR, NPOR, RHOB, RHOC, CASEOD) %>% na.omit()
clusters = kmeans(tmp, centers = 5)
clusters$cluster
tmp['cluster'] = clusters$cluster

tmp %>% melt(id.vars = 'CASEOD') %>%
  ggplot(aes_string(x='CASEOD', y='value')) +
  geom_line() + coord_flip() + scale_x_reverse() +
  facet_grid(.~ variable, scales = "free_x")

# las_file = '/Users/steven/Downloads/DIG_2014_0002/100010303416M400_0001.las'
# newlas = las(las_file)
# View(newlas$las_version)
# newlas$well
# View(newlas$curves)
# head(newlas$log_data)
#
# plot(newlas)
#
# tmp = select(newlas$log_data, GR, NPOR, RHOB, RHOC, CASEOD) %>% na.omit()
# clusters = kmeans(tmp, centers = 5)
# clusters$cluster
# tmp['cluster'] = clusters$cluster
#
# tmp %>% melt(id.vars = 'CASEOD') %>%
#   ggplot(aes_string(x='CASEOD', y='value')) +
#   geom_line() + coord_flip() + scale_x_reverse() +
#   facet_grid(.~ variable, scales = "free_x")

# las("/Volumes/steven/Projects/mva/data/well_phi_exports/100010106426W500.las")

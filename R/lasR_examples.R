lasfile = '/Users/steven/OneDrive/Data/LAS_Normalized/GP00044.LAS'
newlas = las(las_file)
newlas$raw_sections[1]
newlas$well_info
newlas$curves
newlas$log_data

plot(newlas)

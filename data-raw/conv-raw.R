# adding species to conv
# original file at M:/docs/coral_bcg_proc/ignore/MorphoTypes.csv

data(conv)

# enter species and conversion factor here
# reformat
conv <- rbind(conv, c('Dichocoenia stokesi', 2))
conv <- data.frame(conv, stringsAsFactors = F)
conv$conv <- as.numeric(conv$conv)
conv <- conv[order(conv$spec), ]
row.names(conv) <- 1:nrow(conv)

# save
save(conv, file = 'data/conv.RData', compress = 'xz')

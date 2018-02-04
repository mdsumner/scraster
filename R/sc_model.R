# quadmesh::quadmesh
# library(raster)
# r <- raster(matrix(1:12, 3))
# library(tidyverse)
# d <- as_tibble(quadmesh:::edgesXY(r))
# d$i <- seq_len(nrow(d))
# ggplot(d, aes(Var1, Var2, label = i)) + geom_text()

#library(quadmesh)
#qm <- quadmesh(r)
#wrapq <- function(x) c(x, x[1L])
#purrr::map_df(purrr::transpose(as_tibble(t(qm$ib))), ~as_tibble(prs(wrapq(unlist(.x)))))

#' object-value for cell-based raster (discrete pixels)
#' (continuous pixels store values with coordinates)
sc_object.RasterLayer <- function(x, ...) {
  tibble::as_tibble(stats::setNames(list(seq_len(ncell(x)), values(x)), c("object_", names(x))))
}


#' corner coordinates
sc_coord.RasterLayer <- function(x, ...) {
  tibble::as_tibble(quadmesh:::edgesXY(x))
}

#' @examples
#' r <- raster(matrix(1:2, 3))
#' sc_object(r)
#' sc_coord(r)
#' sc_path(r)
sc_path.RasterLayer <- function(x, ...) {
  tibble::tibble(ncoords_ = 4, path = silicate::sc_uid(raster::ncell(x)))
}

wrapq <- function(x) c(x, x[1L])
sc_edge.BasicRaster <- function(x, ...) {
  purrr::map_df(purrr::transpose(as_tibble(mesh_index(x))), ~as_tibble(pairs_index(wrapq(unlist(.x)))))
}

#' the topology
#'
#'

## from quadmesh:::prs
pairs_index <- function (x) {
  cbind(head(x, -1L), tail(x, -1L))
}
## from quadmesh:::p4
pair_four <- function (xp, nc)
{
  (xp + c(0L, 0L, rep(nc, 2L)))[c(1L, 2L, 4L, 3L)]
}

## mesh from quadmesh::quadmesh
mesh_index <- function(x, ...) {
  x <- x[[1L]]
  #exy <- sc_coord(x)
  ind <- apply(pairs_index(seq_len(ncol(x) + 1L)), 1L, pair_four, nc = ncol(x) + 1L)
  ind0 <- as.integer(as.vector(ind) + rep(seq(0L, length = nrow(x), by = ncol(x) + 1L), each = 4L * ncol(x)))
  ind1 <- t(matrix(ind0, nrow = 4L))
  #tibble::as_tibble(ind1)
  ind1
}

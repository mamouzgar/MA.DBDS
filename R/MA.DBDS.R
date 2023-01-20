##' Author: Meelad Amouzgar
##' Performs KNN density based downsampling in a multi-dimensional dataset
##' @author Meelad Amouzgar
##'
##'
##'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 theme
#' @import ggplot2
#'
#'
#' @importFrom RANN nn2
#' @importFrom BBmisc normalize
#' @importFrom dplyr %>%
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom dplyr sample_n
#' @importFrom viridis scale_color_viridis
#' @importFrom MASS kde2d






## calculate nearest neighbors
## adapted from FLOWMAP: https://rdrr.io/github/zunderlab/FLOWMAP/src/R/build-graph.R
## input.data: your input data
## features: character vector of features
## k: number of neighbors
## output is a list object containing 2 elements from the RANN::nn2 function: a matrix of NN indices and a matrix of NN euclidiean distances
#' @title performNNsearch
#' @description performNNsearch
#' @noRd
performNNsearch = function(input.data, features, k, radius ){
     # print(length(features))
     MY_INPUT = input.data %>% dplyr::select(any_of(features))
     message('Performing NN search on ', length(features), ' features using ', k, ' neighbors...')
     nns <- RANN::nn2(data=MY_INPUT, k=k+1, searchtype="priority", eps=0.1, radius = radius)
     temp_nnids.df <- as.data.frame(nns$nn.idx)
     temp_nndists.df <- as.data.frame(nns$nn.dists)
     nn.ids.df <- temp_nnids.df[,2:length(temp_nnids.df)]
     nn.dists.df <- temp_nndists.df[,2:length(temp_nndists.df)]
     nns.res = list(nn.ids.df = nn.ids.df, nn.dists.df = nn.dists.df)
     message('NN search complete...')
     return(nns.res)
}


## calculate knn density
## adapted from FLOWMAP: https://rdrr.io/github/zunderlab/FLOWMAP/src/R/build-graph.R
## k is the # of neighbors.
## min = minimum - minimum number of edges allotted based on density, affects connectivity, recommended default is 2
## max = maximum - maximum number of edges allotted based on density, affects connectivity, recommended default is 5
## n = Number of KNN EDGES
# min(densities.df)
#' @title KnnDensity
#' @description KnnDensity
#' @noRd
KnnDensity <- function(k,
                       # min = 2, max =5 ,
                       min_value, max_value,
                       # n,
                       nn.ids, nn.dists
                       #table.lengths, table.breaks, offset
) {
     # print(k)
     all.densities.list <- list()
     for(i in 1:nrow(nn.ids)) {
          ## Alternatives for calculating density
          ## Dist to kth nearest neighbor:
          #density.by.knn <- abs(nn.dists[i,k])
          ## Transformed metric from X.shift paper:
          #density.by.Xshift <- (1/(n*(longest.dist^d))) * (sum(c(1:k)^d)/sum(compile.dists))^d
          ## Sum of distances from 1:kth nearest neighbor:
          #density.by.knn <- sum(abs(nn.dists[i,1:k]))
          ## Mean of distances from 1:kth nearest neighbor:
          density.by.knn <- sum(abs(nn.dists[i,1:k]))/k
          all.densities.list[[paste(i,".nn", sep='')]] <- density.by.knn
     }
     message('density calculated...')
     densities.df <- rbind(matrix(unlist(all.densities.list), byrow=T))
     message('normalizing densities...')
     normalized.densities <- BBmisc::normalize(densities.df, method = "range", range = c(min_value, max_value), margin = 2, on.constant = "quiet")

     return(normalized.densities)
}


## wrapper function to perform KNN search and calculate density. Outputs normalized density values
#' @title calculateKNN_Densities
#' @description calculateKNN_Densities
#' @noRd
calculateKNN_Densities= function(input.data, features, k,  radius ,min_value, max_value){
     nn.res = performNNsearch(input.data, features, k, radius)
     normalized.densities <- KnnDensity(k = k,
                                        # min = 1, max = 100,
                                        # n=0,
                                        min_value = min_value, max_value = max_value,
                                        nn.ids = nn.res$nn.ids.df,
                                        nn.dists = nn.res$nn.dists.df)
     normalized.densities = 1/normalized.densities
     return(normalized.densities)
}

## wrapper function to perform KNN search and calculate density and sample. Outputs downsampled input data

#' @title performKNN_DensityBasedDownsampling
#' @description This function performs KNN DBDS
#' @param input.data  your dataset
#' @param downsample_n # of observations to downsample to
#' @param features vector of feautures to use for KNN search. Features must be in the input.data
#' @param k # of neighbors used in NN search
#' @param radius Radius of search for searchtype='radius'
#' @param min_value minimum value used for normalization range
#' @param max_value minimum value used for normalization range
#' @keywords internal
#' @export
performKNN_DensityBasedDownsampling = function(input.data, downsample_n = 500, features, k = 25, radius = 0, min_value  = 1, max_value = 500){
     normalized.densities = calculateKNN_Densities(input.data, features, k, radius, min_value, max_value)

     message('performing density-based downsampling...')
     input.data$density = normalized.densities
     output.data = input.data%>%
          mutate(probability = (1/density/sum(1/density)))
     output.data = output.data[ sample(1:nrow(output.data), size = downsample_n, replace=FALSE,prob=output.data$probability), ]
     message('complete!')
     return(output.data)
}


#' @title generate_example
#' @description generate_example
#' @export
generate_example = function(){

     # Generate Y-shape data
     x1 <- rnorm(200, mean = 0, sd = 1)
     x2 <- rnorm(200, mean = 3, sd = 1)
     x3 <- rnorm(200, mean = 4, sd = 1)
     x4 <- rnorm(200, mean = 5, sd = 1)
     x5 <- rnorm(200, mean = 6, sd = 1)
     x6 <- rnorm(200, mean = 7.5, sd = 1)



     y1 <- rnorm(200, mean = 0, sd = 1)
     y2 <- rnorm(200, mean = 3, sd = 1)
     y3 <- rnorm(200, mean = 8, sd = 1)
     y4 <- rnorm(200, mean = 5, sd = 1)
     y5 <- rnorm(200, mean = 4, sd = 1)
     y6 <- rnorm(200, mean = 0.75, sd = 1)

     # Combine data
     x <- c(x1, x2, x3, x4, x5, x6)
     y <- c(y1, y2, y3, y4, y5, y6)

     # Plot data
     # plot(x, y, pch = 20, col = "blue")
     df = data.frame(x,y)

     df = df %>% mutate(density = get_density(x =x,  y=y , n = 50), group = 'Example data')
     df_dds_noDBDS = df %>% sample_n(500) %>% mutate(density = get_density(x =x,  y=y , n = 50), group = 'No DBDS')
     df_dds = performKNN_DensityBasedDownsampling(df, features = c('x','y'), downsample_n = 500, k = 50, radius = 0)%>%
          mutate(density = get_density(x =x,  y=y , n = 50), group = 'DBDS')

     df_dds_plot = df_dds_noDBDS %>%
          bind_rows(df_dds ) %>%
          bind_rows(df) %>%
          mutate(group = factor(group, levels = c('Example data', 'No DBDS','DBDS')))
     p.dbds = ggplot(df_dds_plot, aes(x =x,y =y , color =density)) +
          geom_point() +
          viridis::scale_color_viridis(option = 'inferno')+
          facet_wrap(~group)

     print(p.dbds)

     return(df_dds_plot)
}


## adapted from https://slowkow.com/notes/ggplot2-color-by-density/
#' @description get_density
#' @noRd
get_density <- function(x, y, ...) {
     dens <- MASS::kde2d(x, y, ...)
     ix <- findInterval(x, dens$x)
     iy <- findInterval(y, dens$y)
     ii <- cbind(ix, iy)
     return(dens$z[ii])
}


##example:
# ex = data.frame(x = rnorm(10000, 1, 2), y = rnorm(10000, 1, 2))
# ex.ds = performKNN_DensityBasedDownsampling(ex, c('x','y'), k= 5, num_cells = 50, min_value = 1, max_value = 10)





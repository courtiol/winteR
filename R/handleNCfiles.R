#' Loads one temperature prediction grid stored as NC file
#'
#' This function should not be called by the user. It is called internally by
#' [build_source_stars()]. It reads the content of a NC (NetCDF) file and offer the possibility to
#' crop and downsample it. Note that NC files we use here as input are already cropped but they
#' still require to be cropped during reading to make sure the boundaries are defined properly.
#'
#' @inheritParams arguments
#'
#' @return a stars object
#' @export
#'
#' @seealso [load_NC_files()], [build_source_stars()]
#'
#' @examples
#' run <- FALSE
#' if (run) {
#'   path_NC <- "../NC/ISIMIP_sources/gswp3-w5e5/OBSCLIM/gswp3-w5e5_obsclim_tas_lat27.0to72.0lon-13.0to56.0_daily_2011_2019.nc"
#'   obsclim_2011_2019 <- load_NC_file(path_NC)
#'   plot(obsclim_2011_2019[,,,1], axes = TRUE) ## print map for day 1
#' }
#'
load_NC_file <- function(filename,
                         downsample = FALSE, crop = TRUE,
                         .downsampling_args = c(5, 5, 1),
                         .crop_args = list(S = 27, N = 72, W = -13, E = 56)) {

  print(paste("loading nc file", filename))

  r <- stars::read_stars(filename)
  data <- stars::st_as_stars(r,  raster = c("lon", "lat", "time"))
  sf::st_crs(data) <- "+proj=longlat +datum=WGS84 +no_defs "

  if (crop) {
    limits <- sf::st_bbox(c(xmin = .crop_args$W,
                            xmax = .crop_args$E,
                            ymin = .crop_args$S,
                            ymax = .crop_args$N), crs = sf::st_crs(data))
    data <- data[limits]
  }

  if (downsample) {
    data <- stars::st_downsample(data, .downsampling_args)
  }

  data[[1]] <- units::set_units(data[[1]], "degree_Celsius")

  data
}


#' Loads a series of temperature prediction grids stored as NC files
#'
#' This function should not be called by the user. It is called internally by
#' [build_source_stars()]. It function calls [load_NC_file()] repeatedly on several NC files and
#' combine their output. This is required because [the Inter-Sectoral Impact Model Intercomparison
#' Project](https://www.isimip.org/) provides data in slices of years.
#'
#' This function handles parallel processing but not that this requires a huge amount of memory.
#'
#' @inheritParams arguments
#'
#' @return a stars object
#' @export
#'
#' @seealso [load_NC_file()], [build_source_stars()]
#'
#' @examples
#' run <- FALSE
#' if (run) {
#'   path_NC_dir <- "../NC/ISIMIP_sources/gswp3-w5e5/OBSCLIM/"
#'   obsclim <- load_NC_files(path_NC_dir, downsample = TRUE)
#'   obsclim
#'  }
#'
load_NC_files <- function(directory_NCfiles,
                          downsample = FALSE, crop = TRUE,
                          .downsampling_args = c(5, 5, 0),
                          .crop_args = list(S = 27, N = 72, W = -13, E = 56),
                          nb_cores = 2
                          ) {

  filenames <- dir(path = directory_NCfiles, pattern = "*.nc", full.names = TRUE)

  max_possible_nb_cores <- parallel::detectCores()
  if (nb_cores > max_possible_nb_cores) {
    warning(paste("`nb_cores` reduced to the maximum value supported by your computer, i.e."), max_possible_nb_cores)
    nb_cores <- max_possible_nb_cores
  }

  if (length(filenames > 1)) {
     if (nb_cores > 1) {
        all_data_list <- parallel::mclapply(filenames,
                                            function(filename) load_NC_file(filename = filename,
                                                                            downsample = downsample, crop = crop,
                                                                            .downsampling_args =  .downsampling_args, .crop_args = .crop_args),
                                            mc.cores = min(c(nb_cores, length(filenames))))
     } else {
       all_data_list <- lapply(filenames,
                               function(filename) load_NC_file(filename = filename,
                                                               downsample = downsample, crop = crop,
                                                               .downsampling_args =  .downsampling_args, .crop_args = .crop_args))
    }
    all_data <- do.call("c", all_data_list)
  } else {
    all_data <- load_NC_file(filename = filenames,
                             downsample = downsample, crop = crop,
                             .downsampling_args = .downsampling_args, .crop_args = .crop_args)
  }
  all_data
}


#' Create stars object from NC files and store them on hard drive as RDS files
#'
#' This function is used to create all stars objects storing the daily temperature data, which is
#' required to compute the hibernation niche. We do not provide the raw NC files since they are
#' enormous, but we downloaded them from [the Inter-Sectoral Impact Model Intercomparison
#' Project](https://www.isimip.org/) using the python script we wrote (look for
#' download_NC_files_isimip.py). The interface to ISIMIP may change, so please refer to instruction
#' on [https://www.isimip.org/](https://www.isimip.org/).
#'
#'
#' @inheritParams arguments
#'
#' @return nothing (creates files on hard drive)
#' @export
#'
#' @seealso [load_NC_file()], [load_NC_files()]
#'
#' @examples
#' #See ?winteR
#'
build_source_stars <- function(metadirectory_NCfiles,
                               directory_stars,
                               downsample = FALSE, crop = TRUE,
                               .downsampling_args = c(5, 5, 1),
                               .crop_args = list(S = 27, N = 72, W = -13, E = 56),
                               nb_cores = 2) {

  for (model in dir(metadirectory_NCfiles)) {

    for (scenario in dir(paste0(metadirectory_NCfiles, model))) {

      print(paste0("loading files for model `", model, "` scenario `", scenario, "`"))

      stars_obj <- load_NC_files(directory_NCfiles = paste0(metadirectory_NCfiles, model, "/", scenario),
                                 downsample = downsample,
                                 crop = crop,
                                 .downsampling_args = .downsampling_args,
                                 .crop_args = .crop_args,
                                 nb_cores = nb_cores)

      names(stars_obj) <- "temp"
      gc()

      print(paste0("saving rds for ", model, "_", scenario))

      if (!dir.exists(directory_stars)) dir.create(directory_stars)

      saveRDS(stars_obj,
              file = paste0(directory_stars, model, "_", scenario, ".rds"),
              compress = FALSE)
      rm(stars_obj)
      gc()
    }
  }
  print("done")
  NULL
}

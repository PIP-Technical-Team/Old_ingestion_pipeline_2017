
library(targets)
library(tarchetypes)
library(gittargets)


if (requireNamespace("gert", quietly = TRUE)) {
  library(gert)
  gca <- function(x, ...) {
    gert::git_commit_all(x, ...)
  }
  
  gp <- function(x = NULL, ...) {
    gert::git_push(x, ...)
  }
  
  ga <- function(...) {
    gert::git_add(gert::git_status(...)$file)
  }
  
  gi <- function() {
    gert::git_info()$upstream
  }
  gs <- function() {
    gert::git_status()
  }
}

if (requireNamespace("pushoverr", quietly = TRUE)) {
  
  
  run_tar <- function(...) {
    # names <- rlang::enquo(names)
    s     <- Sys.time()
    start <-  format(s, "%H:%M")
    
    msg <- tryCatch(
      expr = {
        # Your code...
        tar_make(...)
        
        f      <- Sys.time()
        finish <- format(f, "%H:%M")
        
        d <- f - s
        
        msg <- paste0("SUCCESS in pipeline. \nStarted at ", start,
                      "\nFinished at ", finish,
                      "\nDifference ", d)
        
        tar_load_globals()
        
        sync_status <- syncdr::compare_directories(
          left_path  = fs::path(gls$OUT_DIR_PC, gls$vintage_dir),
          right_path = fs::path("e:/PIP/pipapi_data", gls$vintage_dir) |>
            fs::dir_create(),
          by_date    = TRUE,
          by_content = FALSE,
          verbose    = FALSE,
          recurse    = TRUE)
        
        sync_common_files <- syncdr::common_files_asym_sync_to_right(
          sync_status = sync_status,
          force       = TRUE,
          verbose     = FALSE)
        
        sync_uncommon_files <- syncdr::update_missing_files_asym_to_right(
          sync_status     = sync_status,
          copy_to_right   = TRUE,
          delete_in_right = FALSE,
          exclude_delete  = c("cache.duckdb", # file
                              "lineup_data", # folder
                              "prod_refy_estimation.fst",
                              "lineup_dist_stats.fst",
                              "lineup_years.fst"),
          force           = TRUE,
          verbose         = FALSE)
        msg
      }, # end of expr section
      
      error = function(e) {
        f      <- Sys.time()
        finish <- format(f, "%H:%M")
        
        d <- f - s
        
        paste0("ERROR in pipeline. \nStarted at ", start,
               "\nFinished at ", finish,
               "\nDifference ", d)
        
      }, # end of error section
      
      warning = function(w) {
        f      <- Sys.time()
        finish <- format(f, "%H:%M")
        
        d <- f - s
        tar_load_globals()
        
        sync_status <- syncdr::compare_directories(
          left_path  = fs::path(gls$OUT_DIR_PC, gls$vintage_dir),
          right_path = fs::path("e:/PIP/pipapi_data", gls$vintage_dir) |>
            fs::dir_create(),
          by_date    = TRUE,
          by_content = FALSE,
          verbose    = FALSE,
          recurse    = TRUE)
        
        sync_common_files <- syncdr::common_files_asym_sync_to_right(
          sync_status = sync_status,
          force       = TRUE,
          verbose     = FALSE)
        
        sync_uncommon_files <- syncdr::update_missing_files_asym_to_right(
          sync_status     = sync_status,
          copy_to_right   = TRUE,
          delete_in_right = FALSE,
          exclude_delete  = c("cache.duckdb", # file
                              "lineup_data", # folder
                              "prod_refy_estimation.fst",
                              "lineup_dist_stats.fst",
                              "lineup_years.fst"),
          force           = TRUE,
          verbose         = FALSE)
        
        paste0("WARNING in pipeline. \nStarted at ", start,
               "\nFinished at ", finish,
               "\nDifference ", d)
        
      } # end of finally section
      
    ) # End of trycatch
    pushoverr::pushover(msg)
    cli::cli_alert(msg)
    
    
    return(invisible(TRUE))
  }
  
}


# ---- Tiny helpers (expose a few, keep rest optional) ----
if (Sys.info()[["user"]] == "wb384996") {
  tdirp <- fs::path("p:/02.personal/wb384996/temporal/R/")
  tdire <- fs::path("E:/PovcalNet/01.personal/wb384996/temp/R")
}


postInstallFixes <- function(folderToFix) {
  if(length(ls(all.names=TRUE,pattern="\\..postProcessLibraryModule")) > 0 ) #We seem to be running in JASP
  {
    #print("we are *in* jasp, so we use .postProcessLibraryModule!")
    .postProcessLibraryModule(folderToFix)
  }
  else 
  {
    #We do not have that function available so we will need to start JASPEngine ourselves, but where is it?
    jaspEngineLocation <- Sys.getenv("JASPENGINE_LOCATION", unset = file.path(getwd(), "..", "JASPEngine"))
    jaspEngineCall     <- paste0(jaspEngineLocation, ' "', folderToFix ,'"')
    #print(paste0("Calling JASPEngine as: '", jaspEngineCall ,"'"))
    system(jaspEngineCall)
  }
}

installJaspModule <- function(modulePkg, libPathsToUse, moduleLibrary, repos, onlyModPkg, force = FALSE) {
  assertValidJASPmodule(modulePkg)

  r <- getOption("repos")
  r["CRAN"] <- repos
  options(repos = r)

  if (!(force || md5SumsChanged(modulePkg, moduleLibrary))) {
    moduleName <- getModuleInfo(modulePkg)[["Package"]]
    if (dir.exists(file.path(moduleLibrary, moduleName))) {
      print(sprintf("Nothing changed according to md5sums, not reinstalling %s.", moduleName))
      return("succes!")
    } else {
      print(sprintf("Checksums exist for %s but the package is missing, installing anyway!", moduleName))
    }
  }

  return(
    pkgbuild::with_build_tools( 
    {
      if (hasRenvLockFile(modulePkg)) installJaspModuleFromRenv(       modulePkg, libPathsToUse, moduleLibrary, repos, onlyModPkg)
      else                            installJaspModuleFromDescription(modulePkg, libPathsToUse, moduleLibrary, repos, onlyModPkg)
    }, 
    required=FALSE )
  )
}



installJaspModuleFromRenv <- function(modulePkg, libPathsToUse, moduleLibrary, repos, onlyModPkg, prompt = interactive()) {

  print(sprintf("Installing module with renv. installJaspModuleFromRenv('%s', c(%s), '%s', '%s', %s)",
                modulePkg, paste0("'", libPathsToUse, "'", collapse = ", "), moduleLibrary, repos, onlyModPkg))

  renv::consent() #Consent to doing stuff in certain paths: https://rstudio.github.io/renv/reference/consent.html We've changed at least the root and the cache so hopefully that means it won't be changing stuff in the locations it mentions
  options(renv.verbose=TRUE) # More feedback wanted although this seems to do little
  options(renv.config.install.verbose=TRUE)

  if (!dir.exists(moduleLibrary))
    if (!dir.create(moduleLibrary))
      stop("failed to create moduleLibrary!")

  # ensure this starts with a ., otherwise it's picked up by renv "helpers"
  moduleLibraryTemp <- file.path(moduleLibrary, ".renv_temp")

  if (!dir.exists(file.path(moduleLibraryTemp, "renv")))
    dir.create(   file.path(moduleLibraryTemp, "renv"), recursive = TRUE)

  lockFileModule <- file.path(modulePkg,         "renv.lock")
  lockFileTemp   <- file.path(moduleLibraryTemp, "renv.lock")
  file.copy(from = lockFileModule, to = lockFileTemp, overwrite = TRUE)

  setupRenv(moduleLibrary)

  # TODO: unclear whether this is necessary within JASP, or just within Rstudio.
  # renv must be unaware of any other libPaths than the cache and the directory designated for the module.
  # if there are other libPaths then renv may reuse pkgs from those other libPaths
  # it does copy those pkg to the cache before symlinking them
  # inspired by https://stackoverflow.com/a/36873741/4917834
  # it does appear to be necessary within rstudio and when the pkgs from jasp-required-files are present in a libPath
  # old.lib.loc <- .libPaths()
  # on.exit(assign(".lib.loc", old.lib.loc,   envir = environment(.libPaths)))
  #         assign(".lib.loc", moduleLibrary, envir = environment(.libPaths))


  lib <- renv::paths[["library"]](project = moduleLibrary)
  if (!dir.exists(lib))
    dir.create(lib, recursive = TRUE)
  print("before restore")
  
  # assignFunctionInPackage(
  #   fun     = function (fmt = "", ..., con = stdout()) {
  #     if (fmt == "Retrieving '%s' ...")
  #       print("yo I'm here!")
  #     print(sprintf(fmt, ...))
  #     # if (!is.null(fmt) && renv:::renv_verbose()) 
  #     #   writeLines(sprintf(fmt, ...), con = con)
  #   },
  #   name    = "vwritef",
  #   package = "renv"
  # )
  # 
  # assignFunctionInPackage(
  #   fun     = renv_restore_run_actions,
  #   name    = "renv_restore_run_actions",
  #   package = "renv"
  # )
  # 
  # assignFunctionInPackage(
  #   fun     = renv_retrieve_impl,
  #   name    = "renv_retrieve_impl",
  #   package = "renv"
  # )
  
  # this doesn't do anything, but without it the installation of modules with a lockfile crashes...
  assignFunctionInPackage(
    fun     = renv_remotes_resolve_path,
    name    = "renv_remotes_resolve_path",
    package = "renv"
  )
  
  renv::restore(project  = moduleLibraryTemp,
                library  = moduleLibrary,
                lockfile = lockFileTemp, clean = TRUE,
                prompt   = prompt)
  print("after restore")
  moduleInfo         <- getModuleInfo(modulePkg)
  correctlyInstalled <- installModulePkg(modulePkg, moduleLibrary, prompt, moduleInfo)

  if (correctlyInstalled)
    writeMd5Sums(modulePkg, moduleLibrary)

  renv::snapshot(
    project  = moduleLibraryTemp,
    lockfile = lockFileTemp,
    packages = moduleInfo[["Package"]],
    prompt   = prompt,
    force    = TRUE # force is "safe" here because we only update the new module
  )

  # some checks here to assert that everything got installed correctly
  if (!libraryMatchesLockfile(moduleLibraryTemp)) {

    # do it again!
    renv::restore(
      project  = moduleLibraryTemp,
      library  = moduleLibrary,
      lockfile = lockFileTemp,
      clean    = TRUE,
      prompt   = prompt
    )

    if (!libraryMatchesLockfile(moduleLibraryTemp))
      warning("Failed to recreate lock file of module!")

  }

  if (unlink(moduleLibraryTemp, recursive = TRUE)) # 0/ FALSE for success
    warning(sprintf("Failed to remove temporary module libary at %s", moduleLibraryTemp))

  return("succes!")
}

renv_download <- function (url, destfile, type = NULL, quiet = FALSE, headers = NULL) {
  override <- getOption("renv.download.override")
  if (is.function(override)) {
    result <- renv:::catch(renv:::override(url = url, destfile = destfile, 
                             quiet = quiet, mode = "wb", headers = headers))
    if (inherits(result, "error")) 
      renv:::renv_download_error(result, "%s", renv:::conditionMessage(result))
    return(destfile)
  }
  if (quiet) 
    renv:::renv_scope_options(renv.verbose = FALSE)
  url <- chartr("\\", "/", url)
  destfile <- chartr("\\", "/", destfile)
  renv:::vwritef("Retrieving '%s' ...", url)
  print("after retrieving")
  headers <- c(headers, renv:::renv_download_custom_headers(url))
  print("headers")
  print(headers)
  print("trying renv:::renv_download_local")
  if (renv:::renv_download_local(url, destfile, headers)) 
    return(destfile)
  renv:::renv_scope_downloader()
  info <- file.info(destfile, extra_cols = FALSE)
  if (identical(info$isdir, FALSE)) {
    size <- renv:::renv_download_size(url, type, headers)
    if (info$size == size) {
      print("file is ok?")
      renv:::vwritef("\tOK [file is up to date]")
      return(destfile)
    }
  }
  print("didn't return early")
  callback <- renv:::renv_file_backup(destfile)
  on.exit(renv:::callback(), add = TRUE)
  tempfile <- renv:::renv_tempfile_path(tmpdir = dirname(destfile))
  before <- Sys.time()
  print("doing a different download")
  status <- renv:::renv_download_impl(url = url, destfile = tempfile, 
                               type = type, request = "GET", headers = headers)
  after <- Sys.time()
  if (inherits(status, "error")) 
    renv:::renv_download_error(url, "%s", renv:::conditionMessage(status))
  if (status != 0L) 
    renv:::renv_download_error(url, "error code %i", status)
  if (!file.exists(tempfile)) 
    renv:::renv_download_error(url, "%s", "unknown reason")
  status <- renv:::renv_download_check_archive(tempfile)
  if (inherits(status, "error")) 
    renv:::renv_download_error(url, "%s", "archive cannot be read")
  renv:::renv_download_report(after - before, tempfile)
  renv:::renv_file_move(tempfile, destfile)
  print("got to the end of the download function")
  destfile
}

renv_restore_run_actions <- function (project, actions, current, lockfile, rebuild) {
  packages <- names(actions)
  renv:::renv_scope_restore(project = project, library = renv:::renv_libpaths_default(), 
                     records = renv:::renv_records(lockfile), packages = packages, 
                     rebuild = rebuild)
  removes <- actions[actions == "remove"]
  renv:::enumerate(removes, function(package, action) {
    renv:::renv_restore_remove(project, package, current)
  })
  installs <- actions[actions != "remove"]
  packages <- names(installs)
  print("before renv:::renv_retrieve")
  records <- renv:::renv_retrieve(packages)
  print("before renv:::renv_install")
  status <- renv:::renv_install(records)
  diff <- renv:::renv_lockfile_diff_packages(renv:::renv_records(lockfile), 
                                      records)
  diff <- diff[diff != "remove"]
  if (!renv:::empty(diff)) {
    renv:::renv_pretty_print_records(records[names(diff)], "The dependency tree was repaired during package installation:", 
                              "Call `renv::snapshot()` to capture these dependencies in the lockfile.")
  }
  renv:::renv_install_postamble(names(records))
  invisible(records)
}

# renv:::renv_retrieve_impl
renv_retrieve_impl <- function (package) {
  `%||%` <- renv:::`%||%`
  if (package %in% renv:::renv_packages_base()) 
    return()
  state <- renv:::renv_restore_state()
  if (renv:::visited(package, envir = state$retrieved)) 
    return()
  records <- state$records
  record <- records[[package]] %||% renv:::renv_retrieve_missing_record(package)
  source <- renv:::renv_record_source(record, normalize = TRUE)
  ostype <- tolower(record[["OS_type"]] %||% "")
  skip <- renv:::renv_platform_unix() && identical(ostype, "windows") || 
    renv:::renv_platform_windows() && identical(ostype, "unix")
  print(sprintf("skip is %s, source is %s", skip, source))
  if (skip) 
    return()
  print("still here 0")
  if (source %in% c("bioconductor")) 
    renv:::renv_scope_bioconductor()
  print("still here 1")
  if (renv:::renv_retrieve_incompatible(record)) 
    record <- renv:::renv_available_packages_latest(package)
  print("still here 2")
  uselatest <- source %in% c("repository", "bioconductor") && 
    is.null(record$Version)
  print("still here 3")
  if (uselatest) 
    record <- renv:::renv_available_packages_latest(record$Package)
  print("still here 4")
  if (!renv:::renv_restore_rebuild_required(record)) {
    path <- renv:::renv_restore_find(record)
    if (file.exists(path)) {
      print("!renv:::renv_restore_rebuild_required(record) and path exists")
      return(renv:::renv_retrieve_successful(record, path, install = FALSE))
    }
    cacheable <- renv:::renv_cache_config_enabled(project = state$project) && 
      renv:::renv_record_cacheable(record)
    if (cacheable) {
      path <- renv:::renv_cache_find(record)
      if (renv:::renv_cache_package_validate(path)) {
        print("successful renv:::renv_cache_package_validate")
        return(renv:::renv_retrieve_successful(record, path))
      }
    }
  }
  print("still here 5")
  path <- record$Path %||% ""
  if (file.exists(path)) {
    print("path exists")
    return(renv:::renv_retrieve_successful(record, path))
  }
  print("still here 6")
  if (!renv:::renv_restore_rebuild_required(record)) {
    shortcuts <- c(renv:::renv_retrieve_explicit, renv:::renv_retrieve_local, 
                   if (!renv:::renv_tests_running() && renv:::config$install.shortcuts()) renv:::renv_retrieve_libpaths)
    print("still here 7")
    i <- 0
    for (shortcut in shortcuts) {
      i <- i + 1
      print(sprintf("trying shortcut %s", i))
      retrieved <- renv:::catch(shortcut(record))
      if (identical(retrieved, TRUE)) {
        print("identical(retrieved, TRUE) was TRUE")
        return(TRUE)
      }
    }
  }
  print("entering the switch")
  switch(
    source,
    bioconductor = renv:::renv_retrieve_bioconductor(record),
    bitbucket = renv:::renv_retrieve_bitbucket(record),
    git = renv:::renv_retrieve_git(record),
    github = renv:::renv_retrieve_github(record),
    gitlab = renv:::renv_retrieve_gitlab(record),
    repository = renv:::renv_retrieve_repos(record),
    url = renv:::renv_retrieve_url(record),
    renv:::renv_retrieve_unknown_source(record)
  )
}

renv_remotes_resolve_path <- function() {
  # print("renv_remotes_resolve_path 0")
  path <- renv:::renv_path_normalize(entry, winslash = "/", mustWork = TRUE)
  # print("renv_remotes_resolve_path 1")
  if (renv:::renv_archive_type(entry) %in% c("tar", "zip")) 
    return(renv:::renv_remotes_resolve_path_impl(path))
  # print("renv_remotes_resolve_path 2")
  if (renv:::renv_project_type(path) == "package") 
    return(renv:::renv_remotes_resolve_path_impl(path))
  # print("renv_remotes_resolve_path 3")
  renv:::stopf("there is no package at path '%s'", entry)
  
}

installJaspModuleFromDescription <- function(modulePkg, libPathsToUse, moduleLibrary, repos, onlyModPkg, prompt = interactive()) {

  print("Installing module with DESCRIPTION file")

  if (!dir.exists(moduleLibrary))
    if (!dir.create(moduleLibrary))
      stop("failed to create moduleLibrary!")

  moduleLibraryTemp <- file.path(moduleLibrary, ".renv_temp")

  if (!dir.exists(file.path(moduleLibraryTemp, "renv")))
    dir.create(file.path(moduleLibraryTemp, "renv"), recursive = TRUE)

  setupRenv(moduleLibrary)

  # make renv blind for other libPaths
  # old.lib.loc <- .libPaths()
  # on.exit(assign(".lib.loc", old.lib.loc, envir = environment(.libPaths)))
  # assign(".lib.loc", moduleLibrary, envir = environment(.libPaths))

  # TODO: this is not very efficient because renv::install looks up the remotes on github...
  # there is a better way but it requires us to mess with renv's internals or to be more explicit about pkgs
  renv::hydrate(library = moduleLibrary, project = modulePkg)
  renv::install(project = modulePkg, library = moduleLibrary, prompt = prompt)

  correctlyInstalled <- installModulePkg(modulePkg, moduleLibrary, prompt)
  if (correctlyInstalled)
    writeMd5Sums(modulePkg, moduleLibrary)

  if (unlink(moduleLibraryTemp, recursive = TRUE)) # 0/ FALSE for success
    warning(sprintf("Failed to remove temporary module libary at %s", moduleLibraryTemp))

  return("succes!")

}

installModulePkg <- function(modulePkg, moduleLibrary, prompt = interactive(), moduleInfo = NULL) {

  if (is.null(moduleInfo))
    moduleInfo <- getModuleInfo(modulePkg)
  record <- recordFromModule(modulePkg, moduleInfo)
  renv::install(record, library = moduleLibrary, rebuild = TRUE, prompt = prompt)
  TRUE

}

assertValidJASPmodule <- function(modulePkg) {

  if (!file.exists(file.path(modulePkg, "DESCRIPTION")))
    stop("Your module is missing a 'DESCRIPTION' file!")

  if (!file.exists(file.path(modulePkg, "inst", "Description.qml")))
    stop("Your module is missing 'inst/Description.qml'!")

  if (!dir.exists(file.path(modulePkg, "R")))
    stop("Your module is missing an 'R' directory!")

  if (!dir.exists(file.path(modulePkg, "inst", "qml")))
    stop("Your module is missing the 'inst/qml' directory!")

}

hasRenvLockFile <- function(modulePkg) {
  return(file.exists(file.path(modulePkg, "renv.lock")))
}

recordFromModule <- function(modulePkg, moduleInfo) {

  record <- list(list(
    Package   = moduleInfo[["Package"]],
    Version   = moduleInfo[["Version"]],
    Path      = modulePkg,
    Source    = "Local",
    Cacheable = TRUE
  ))
  names(record) <- moduleInfo[["Package"]]

  return(record)
}

getModuleInfo <- function(modulePkg) {
  pkgDescr <- file.path(modulePkg, "DESCRIPTION")
  if (!file.exists(pkgDescr))
    stop("Your module contains no DESCRIPTION file")

  return(read.dcf(file.path(modulePkg, "DESCRIPTION"))[1, ])

}

renv_diagnostics_packages_as_df <- function(project) {

  lockfile  <- renv:::renv_diagnostics_packages_lockfile(project)
  libstate  <- renv:::renv_diagnostics_packages_library(project)
  used      <- unique(renv:::renv_diagnostics_packages_dependencies(project)$Package)
  recdeps   <- renv:::renv_package_dependencies(packages = used, project = project)
  all       <- c(names(lockfile$Packages), names(libstate$Packages),
                 names(recdeps))

  renv:::renv_scope_locale(category = "LC_COLLATE", locale = "C")
  
  all                        <- sort(unique(all))
  deps                       <- rep.int(NA_character_, length(all))
  names(deps)                <- all
  deps[names(recdeps)]       <- "indirect"
  deps[used]                 <- "direct"
  libpaths                   <- dirname(renv:::map_chr(all, renv:::renv_package_find))
  flibpaths                  <- factor(libpaths, levels = .libPaths())
  libcodes                   <- as.integer(flibpaths)
  libcodes[!is.na(libcodes)] <- sprintf("[%i]", libcodes[!is.na(libcodes)])

  return(data.frame(
    Library          = renv:::renv_diagnostics_packages_version(libstate, all),
    Source           = renv:::renv_diagnostics_packages_sources(libstate, all),
    Lockfile         = renv:::renv_diagnostics_packages_version(lockfile, all),
    Source           = renv:::renv_diagnostics_packages_sources(lockfile, all),
    Path             = libcodes,
    Dependency       = deps,
    stringsAsFactors = FALSE,
    check.names      = FALSE
  ))
}

libraryMatchesLockfile <- function(project = NULL) {
  project <- renv:::renv_project_resolve(project)
  df      <- renv_diagnostics_packages_as_df(project)
  notNA   <- which(complete.cases(df[, c("Library", "Lockfile")]))
  idxDiff <- which(df[notNA, "Library"] != df[notNA, "Lockfile"])
  hasDiff <- length(idxDiff) > 0L

  if (hasDiff) {
    print("Found the following mismatches between Library and the lockfile!")
    print(df[notNA[idxDiff], ])
  }
   
  return(!hasDiff)
}

addRenvBeforeAfterDispatch <- function() { 

  renBeforeAfterInstallStruct <- structure(list(
    before.install = function(x) {
      #print("BEFORE INSTALLING")
      #print(sprintf("Path = %s", mget("path", envir = parent.frame(1),
      #                                ifnotfound = "unknown path")))
      0 #do nothing
    }, 
    
    after.install = function(x) 
    {
      installPath <- mget("installpath", envir = parent.frame(1), ifnotfound = "unknown")

      if(installPath != "unknown") 
      {
        print(sprintf("Installed %s to '%s', now running post install fixes.", x, installPath))
        postInstallFixes(installPath)
      } 
      else
        print(sprintf("Installing %s failed for some reason...", x))
                                      
    }),
    class = "renvInstallHookOverride"
  )

  options(renv.install.package.options = renBeforeAfterInstallStruct)
}

`[[.renvInstallHookOverride` <- function(x, ...) {
  return(unclass(x))
}

setupRenv <- function(moduleLibrary) {

  # renv adds e.g,. "R-3.6/x86_64-pc-linux-gnu" to all paths (R-version/os) and we don't need that
  assignFunctionInPackage(
    fun     = function() return(""),
    name    = "renv_bootstrap_platform_prefix",
    package = "renv"
  )

  cachePaths <- strsplit(Sys.getenv("RENV_PATHS_CACHE"), .Platform$path.sep)

  for(cachePath in cachePaths)
    if (!dir.exists(cachePath))
     stop(sprintf("A cache is supposed to be at '%s' but it does not exist!", cachePath))

  Sys.setenv("RENV_PATHS_LIBRARY" = moduleLibrary)

  print("Using the following paths:")
  for(name in names(renv::paths))
    print(sprintf("%s:%s%s", name, paste0(rep(" ", 12 - nchar(name)), collapse=""), renv::paths[[name]]()))

  options(install.opts = "--no-docs --no-test-load"); #make sure we do not do a test-load, because this will not work on mac. the rpaths still need to be fixed

  #Try to nudge renv towards installing binaries when possible
   if(getOS() == "windows" || getOS() == "osx")
    options(pkgType = "binary");
  
  if (getOS() == "osx")
    addRenvBeforeAfterDispatch()
  else
    print("did not call addRenvBeforeAfterDispatch")
}

installJaspModuleFromDescriptionOld <- function(modulePkg, libPathsToUse, moduleLibrary, repos, onlyModPkg) {
  pkgDescr <- file.path(modulePkg, "DESCRIPTION")
  if (!file.exists(pkgDescr))
    stop("Your module contains no DESCRIPTION file")

  moduleInfo <- read.dcf(file.path(modulePkg, "DESCRIPTION"))[1, ]
  pkgName    <- moduleInfo["Package"]
  modulePkg  <- paste0(modulePkg, "/.")

  if (!onlyModPkg) {
    # Install dependencies:
    withr::with_libpaths(
      new  = libPathsToUse,
      code = remotes::install_deps(pkg=modulePkg, lib=moduleLibrary,  INSTALL_opts=c("--no-test-load --no-multiarch"), upgrade="never", repos=repos)
    )

    # And fix Mac OS libraries of dependencies:
    postInstallFixes(moduleLibrary)
  }

  # Remove old copy of library (because we might be reinstalling and want the find.package check on the end to fail if something went wrong)
  tryCatch(
    expr = {
      withr::with_libpaths(
        new = libPathsToUse,
        code = {
          find.package(package=pkgName)
          remove.packages(pkg=pkgName, lib=moduleLibrary)
        }
      )
    },
    error=function(e) {}
  )

  print("Module library now looks like: ")
  print(list.files(path=moduleLibrary, recursive=FALSE))

  pkgPath <- sub("\\\\", "/", modulePkg, fixed=TRUE)
  print(paste0("pkgPath: '", pkgPath, "'"))

  strlibPathsToUse  <- paste0("c(", paste0("'", libPathsToUse, collapse = "', "), "')")
  loadLog 			    <- .runSeparateR(paste0("withr::with_libpaths(new=", strlibPathsToUse, ", pkgbuild::with_build_tools(install.packages(pkgs='", pkgPath, "', lib='", moduleLibrary, "', type='source', repos=NULL, INSTALL_opts=c('--no-multiarch')), required=FALSE))"))

  # Check if install worked and through loadlog as error otherwise
  tryCatch(
    expr = {
      withr::with_libpaths(
        new = moduleLibrary,
        code = {
          find.package(package=pkgName)
          return("succes!")
        }
      )
    },
    error=function(e) {
      .setLog(loadLog)
      return('fail')
    }
  )

  return(NULL)
}

# Retrieve package dependencies by parsing a DESCRIPTION file or a DESCRIPTION string
#
# Returns a named list, e.g.:
# type         package      version               remote
# Depends        R          >= 3.0.2
# Imports       stringr     >= 0.5         svn::https://github.com/hadley/stringr
# Imports       brew            *
# Imports       digest          *          foo/digest
# Imports       methods         *
# Imports       Rcpp         >= 0.11.0
# Suggests      testthat     >= 0.8.0      local::/pkgs/testthat
# Suggests      knitr           *
# LinkingTo     Rcpp
#
# Or a json string, e.g.:
# [{"type":"Depends","package":"R","version":">= 3.0.2","remote":""},
#  {"type":"Imports","package":"stringr","version":">= 0.5","remote":"svn::https://github.com/hadley/stringr"},
#  {"type":"Imports","package":"brew","version":"*","remote":""},
#  {"type":"Imports","package":"digest","version":"*","remote":"foo/digest"},
# etc]
getDepsFromDescription <- function(unparsedDescr, asJson = TRUE) {
  deps  <- NULL
  descr <- .parseDescription(unparsedDescr)

  if (any(descr$has_fields(c("Imports", "Depends", "LinkingTo")))) {
    deps         <- descr$get_deps()
    pkgs         <- deps[["package"]]
    remotePerPkg <- character(length(pkgs))

    if (descr$has_fields("Remotes")) {
      remotes <- descr$get_remotes()

      for (i in seq_along(remotePerPkg)) {
        isRemoteForPkg <- endsWith(remotes, paste0("/", pkgs[i]))

        if (any(isRemoteForPkg))
          remotePerPkg[i] <- remotes[isRemoteForPkg]
      }
    }
    deps[["remote"]] <- remotePerPkg
  }

  if (asJson)
    deps <- jsonlite::toJSON(deps)

  return(deps)
}

.parseDescription <- function(unparsedDescr) {
  if (!nzchar(unparsedDescr))
    stop("The description contains no data")

  if (file.exists(unparsedDescr)) return(desc::description$new(file = unparsedDescr))
  else                            return(desc::description$new(text = unparsedDescr))
}

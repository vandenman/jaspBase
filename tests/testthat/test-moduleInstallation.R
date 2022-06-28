# TODO: create descriptive convenience wrappers for
# testthat::expect(utils::file_test("-d", dir) and friends

test_that("package installation works", {

  tempRoot <- tempdir()

  mockJaspRoot    <- normalizePath(testthat::test_path("mock-jasp-desktop"))

  moduleName      <- "jaspDescriptives"
  tempLibPath     <- file.path(tempRoot, "jasp-desktop-library")
  tempBuildFolder <- file.path(tempRoot, "jasp-desktop-build", "Modules")
  moduleLibrary   <- file.path(tempBuildFolder, "Modules", "jaspDescriptives")
  modulePkg       <- file.path(mockJaspRoot, "Modules", moduleName)

  # compute the hashes that are expected in the lockfile
  precomputedHashes <- c(
    "jaspDescriptives" = jaspBase:::getModuleHash(modulePkg),
    "jaspGraphs"       = jaspBase:::getModuleHash(file.path(mockJaspRoot, "Engine", "jaspGraphs")),
    "jaspBase"         = jaspBase:::getModuleHash(file.path(mockJaspRoot, "Engine", "jaspBase"))
  )
  # TODO: test these hashes against hard coded values

  renvRootPath  <- file.path(tempRoot, "renv-root")
  renvCachePath <- file.path(tempRoot, "renv-cache")

  # let's not polute anybodies cache/ root
  tempEnvvars <- c(
    "RENV_PATHS_ROOT"  = renvRootPath,
    "RENV_PATHS_CACHE" = renvCachePath
  )

  # Sys.setenv(RENV_PATHS_ROOT  = renvRootPath)
  # Sys.setenv(RENV_PATHS_CACHE = renvCachePath)

  for (recordPackages in c("localJasp", "all")) {

    mkdirs(tempLibPath, tempBuildFolder, tempBuildFolder, moduleLibrary, renvRootPath, renvCachePath)

    debugonce(jaspBase:::installModuleNew)
    withr::with_envvar(tempEnvvars, {
      jaspBase::installJaspModuleNew(modulePkg = modulePkg, jaspRoot = mockJaspRoot, moduleLibrary = moduleLibrary, recordPackages = recordPackages)

      installedDir <- file.path(moduleLibrary, moduleName)

      expect_dir    (installedDir, failure_message = "Failed to install jaspDescriptives - No folder.")
      expect_symlink(installedDir, failure_message = "Failed to cache jaspDescriptives - Folder is not a symlink.")

      lockfilePath <- file.path(moduleLibrary, "renv.lock")
      expect_file(lockfilePath, "Lockfile for jaspDescriptives does not exist")

      lockfile <- renv:::renv_lockfile_read(lockfilePath)

      moduleDescription <- renv:::renv_description_read(file.path(modulePkg, "DESCRIPTION"))
      moduleVersion <- moduleDescription$Version
      moduleHash <- lockfile$Packages[[moduleName]]$Hash

      testthat::expect_identical(moduleHash, precomputedHashes[[moduleName]], label = sprintf("hash of %s in lockfile does not match the precomputed hash.", moduleName))
      cacheDir <- file.path(renv::paths$cache(), moduleName, moduleVersion, lockfile$Packages[[moduleName]]$Hash, moduleName)
      testthat::expect(utils::file_test("-d", cacheDir), "Failed to cache jaspDescriptives - Folder does not exist in the renv cache")

      for (dep in c("jaspGraphs", "jaspBase")) {
        moduleDescription <- renv:::renv_description_read(file.path(mockJaspRoot, "Engine", dep, "DESCRIPTION"))
        moduleVersion <- moduleDescription$Version
        moduleHash <- lockfile$Packages[[dep]]$Hash

        testthat::expect_identical(moduleHash, precomputedHashes[[dep]], label = sprintf("hash of %s in lockfile does not match the precomputed hash.", dep))

        cacheDir <- file.path(renv::paths$cache(), dep, moduleVersion, lockfile$Packages[[dep]]$Hash, dep)
        testthat::expect(utils::file_test("-d", cacheDir), sprintf("Failed to cache %s - Folder does not exist in the renv cache", dep))
      }

    })

  }

})

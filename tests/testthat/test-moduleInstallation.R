# TODO: create descriptive convenience wrappers for
# testthat::expect(utils::file_test("-d", dir) and friends

test_that("package installation works", {
  # skip("for now")
  tempRoot <- tempdir()
  mockJaspRoot    <- normalizePath(testthat::test_path("mock-jasp-desktop"))
  moduleName      <- "jaspDescriptives"
  tempBuildFolder <- file.path(tempRoot, "jasp-desktop-build")
  moduleLibrary   <- file.path(tempBuildFolder, "Modules", moduleName)
  modulePkg       <- file.path(mockJaspRoot,    "Modules", moduleName)

  # let's not polute anybodies cache/ root
  renvRootPath  <- file.path(tempRoot, "renv-root")
  renvCachePath <- file.path(tempRoot, "renv-cache")
  withr::local_envvar(c("RENV_PATHS_ROOT" = renvRootPath, "RENV_PATHS_CACHE" = renvCachePath))

  # compute the hashes that are expected in the lockfile
  precomputedHashes <- c(
    "jaspDescriptives" = jaspBase:::getModuleHash(modulePkg),
    "jaspGraphs"       = jaspBase:::getModuleHash(file.path(mockJaspRoot, "Engine", "jaspGraphs")),
    "jaspBase"         = jaspBase:::getModuleHash(file.path(mockJaspRoot, "Engine", "jaspBase"))
  )
  expect_identical(precomputedHashes,
    c(jaspDescriptives = "b643f59a7ff78c92939a8a02daf7e293",
      jaspGraphs       = "f75115edd4f93c290cb30d507169bd70",
      jaspBase         = "a9c71629923834192707bb0286c2efa6")
  )


  # for (recordPackages in "all") {
  for (recordPackages in c("localJasp", "all")) {
    # for (recordPackages in c("localJasp")) {

    mkdirs(tempBuildFolder, tempBuildFolder, moduleLibrary, renvRootPath, renvCachePath)

    # debugonce(jaspBase:::installModuleNew)

    jaspBase::installJaspModuleNew(modulePkg = modulePkg, jaspRoot = mockJaspRoot, moduleLibrary = moduleLibrary, recordPackages = recordPackages)

    installedDir <- file.path(moduleLibrary, moduleName)

    expect_dir    (installedDir, failure_message = "Failed to install jaspDescriptives - No folder.")
    expect_symlink(installedDir, failure_message = "Failed to cache jaspDescriptives - Folder is not a symlink.")

    lockfilePath <- file.path(moduleLibrary, "renv.lock")
    expect_file(lockfilePath, "Lockfile for jaspDescriptives does not exist")

    lockfile <- renv:::renv_lockfile_read(lockfilePath)

    if (recordPackages == "all")
      browser()

    moduleDescription <- renv:::renv_description_read(file.path(modulePkg, "DESCRIPTION"))
    moduleVersion <- moduleDescription$Version
    # TODO: for some reason the hash in the cache is not the same as the one in the lockfile!
    moduleHash <- if (recordPackages == "all") {
      precomputedHashes[[moduleName]]
    } else {
      lockfile$Packages[[moduleName]]$Hash
    }
    cacheDir <- file.path(renv::paths$cache(), moduleName, moduleVersion, moduleHash, moduleName)

    hashReference <- if (recordPackages == "localJasp") {
      precomputedHashes[[moduleName]]
    } else {
      renv:::renv_hash_description(file.path(cacheDir, "DESCRIPTION"))
    }

    expect_identical(moduleHash, hashReference, label = sprintf("hash of %s in lockfile (%s) does not match the renv hash.", moduleName, recordPackages))
    expect_dir(cacheDir, sprintf("Failed to cache %s - Folder does not exist in the renv cache", moduleName))

    for (dep in c("jaspGraphs", "jaspBase")) {
      depDescription <- renv:::renv_description_read(file.path(mockJaspRoot, "Engine", dep, "DESCRIPTION"))
      depVersion <- depDescription$Version
      depHash <- if (recordPackages == "all") {
        precomputedHashes[[dep]]
      } else {
        lockfile$Packages[[dep]]$Hash
      }
      cacheDir <- file.path(renv::paths$cache(), dep, depVersion, depHash, dep)

      hashReference <- if (recordPackages == "localJasp") {
        precomputedHashes[[dep]]
      } else {
        renv:::renv_hash_description(file.path(cacheDir, "DESCRIPTION"))
      }

      expect_identical(depHash, hashReference, label = sprintf("hash of %s in lockfile (%s) does not match the renv hash.", moduleName, recordPackages))
      expect_dir(cacheDir, sprintf("Failed to cache %s - Folder does not exist in the renv cache", dep))

      testthat::expect_identical(moduleHash, precomputedHashes[[dep]], label = sprintf("hash of %s in lockfile does not match the precomputed hash.", dep))
      cacheDir <- file.path(renv::paths$cache(), dep, moduleVersion, lockfile$Packages[[dep]]$Hash, dep)
      testthat::expect(utils::file_test("-d", cacheDir), sprintf("Failed to cache %s - Folder does not exist in the renv cache", dep))
    }

  }

})

test_that("package installation recognizes modifications in jasp modules and jasp module dependencies", {
  skip("for now")
  tempRoot <- tempdir()
  mockJaspRoot0   <- normalizePath(testthat::test_path("mock-jasp-desktop"))
  moduleName      <- "jaspDescriptives"
  tempBuildFolder <- file.path(tempRoot, "jasp-desktop-build")
  moduleLibrary   <- file.path(tempBuildFolder, "Modules", moduleName)

  file.copy(mockJaspRoot0, tempRoot, recursive = TRUE, overwrite = TRUE)
  mockJaspRoot    <- file.path(tempRoot, "mock-jasp-desktop")
  modulePkg       <- file.path(mockJaspRoot,    "Modules", moduleName)

  # let's not polute anybodies cache/ root
  renvRootPath  <- file.path(tempRoot, "renv-root")
  renvCachePath <- file.path(tempRoot, "renv-cache")
  withr::local_envvar(c("RENV_PATHS_ROOT" = renvRootPath, "RENV_PATHS_CACHE" = renvCachePath))

  mkdirs(tempBuildFolder, tempBuildFolder, moduleLibrary, renvRootPath, renvCachePath)
  lockfilePath <- file.path(moduleLibrary, "renv.lock")
  installedModulePath <- file.path(moduleLibrary, moduleName)

  # 1. Install the package
  jaspBase::installJaspModuleNew(modulePkg = modulePkg, jaspRoot = mockJaspRoot, moduleLibrary = moduleLibrary)

  lockFile1 <- renv:::renv_lockfile_read(lockfilePath)
  fileinfo1 <- file.info(installedModulePath)

  # 2. Modify jaspGraphs
  jaspGraphsDescriptionPath <- file.path(mockJaspRoot, "Engine", "jaspGraphs", "DESCRIPTION")
  jaspGraphsDescription <- readLines(jaspGraphsDescriptionPath)
  jaspGraphsDescription[4] <- "Version: 0.5.2.14"
  writeLines(paste(jaspGraphsDescription, collapse = "\n"), jaspGraphsDescriptionPath)

  jaspBase::installJaspModuleNew(modulePkg = modulePkg, jaspRoot = mockJaspRoot, moduleLibrary = moduleLibrary)

  lockFile2 <- renv:::renv_lockfile_read(lockfilePath)
  fileinfo2 <- file.info(installedModulePath)

  # 3. Modify jaspDescriptives
  jaspDescriptivesDescriptionPath <- file.path(modulePkg, "DESCRIPTION")
  jaspDescriptivesDescription <- readLines(jaspDescriptivesDescriptionPath)
  jaspDescriptivesDescription[4] <- "Version: 0.15.1"
  writeLines(paste(jaspDescriptivesDescription, collapse = "\n"), jaspDescriptivesDescriptionPath)

  jaspBase::installJaspModuleNew(modulePkg = modulePkg, jaspRoot = mockJaspRoot, moduleLibrary = moduleLibrary)

  lockFile3 <- renv:::renv_lockfile_read(lockfilePath)
  fileinfo3 <- file.info(installedModulePath)

  expect_identical(lockFile1$Packages$jaspGraphs$Version, "0.5.2.13")
  expect_identical(lockFile2$Packages$jaspGraphs$Version, "0.5.2.14")
  expect_identical(lockFile1$Packages$jaspDescriptives$Hash, lockFile2$Packages$jaspDescriptives$Hash)
  expect_identical(fileinfo1, fileinfo2)

  expect_identical(lockFile3$Packages$jaspDescriptives$Version, "0.15.1")
  expect_failure(expect_identical(
    lockFile1$Packages$jaspDescriptives$Version,
    lockFile3$Packages$jaspDescriptives$Version
  ))
  expect_failure(expect_identical(fileinfo1, fileinfo3))

})


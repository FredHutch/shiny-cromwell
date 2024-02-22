

readRFilesAndListPackagesAST <- function(directory) {
    if (!dir.exists(directory)) {
        stop("Directory does not exist or cannot be accessed: ", directory)
    }

    r_files <- list.files(directory, pattern = "\\.R$", full.names = TRUE, ignore.case = TRUE)
    if (length(r_files) == 0) {
        stop("No .R files found in directory: ", directory)
    }

    extractPackages <- function(node) {
        packages <- character()
        
        # Safely check node type and contents
        if (is.call(node)) {
            funcName <- as.character(node[[1]])
            if (funcName %in% c("library", "require", "requireNamespace")) {
                if (length(node) >= 2 && !is.null(node[[2]])) {
                    packageName <- gsub("\"|'", "", deparse(node[[2]]))
                    packages <- c(packages, packageName)
                }
            } else if (funcName == "::" || funcName == ":::") {
                if (length(node) >= 3 && !is.null(node[[2]])) {
                    packageName <- as.character(node[[2]])
                    packages <- c(packages, packageName)
                }
            }
        }

        # Recursively extract from all parts of the node
        if (is.list(node) || is.expression(node)) {
            for (i in seq_along(node)) {
                sub_packages <- extractPackages(node[[i]])
                packages <- c(packages, sub_packages)
            }
        }
        return(packages)
    }
    
    all_packages <- character()
    for (file in r_files) {
        tryCatch({
            expr <- parse(file = file)
            file_packages <- unique(na.omit(extractPackages(expr)))
            all_packages <- c(all_packages, file_packages)
        }, error = function(e) {
            message("Error parsing file: ", file, "\nError message: ", e$message)
        })
    }

    return(unique(all_packages))
}


library(testthat) 

test_that("packages are installed", {
  pkgs_used_in_code <- readRFilesAndListPackagesAST("..")
  installed_pkgnames <- names(installed.packages()[, "Package"])
  passed <- all(pkgs_used_in_code %in% installed_pkgnames)
  if (!passed) {
    print("The following packages are used in the code but not installed:")
    print(pkgs_used_in_code[!pkgs_used_in_code %in% installed_pkgnames])
  }
  expect_true(passed)
})




###-------User input------------------------------------------------------------

# Path to the original R code file
Path_to_code <- "~/path/to/code"

# Time limit for code execution 
time_for_code_to_run <- Inf # in seconds


###-------Install needed packages-----------------------------------------------

# Load necessary libraries
if (!require(base)) {
  install.packages("base")
  library(base)
} else {
  library(base)
}

if (!require(R.utils)) {
  install.packages("R.utils")
  library(R.utils)
} else {
  library(R.utils)
}


###-------Create needed functions-----------------------------------------------

# Function to detect packages in R code using regular expressions
# Arguments:
#   code: R code as a character vector or string
# Returns:
#   A character vector containing the names of packages detected in the code

detect_packages_in_code <- function(code) {
  # Use regular expressions to identify package imports in the code
  library_calls <-
    stringr::str_extract_all(code, "(?<=library\\(|require\\()[^)]+")
  packages <- unique(unlist(library_calls))
  return(packages)
}



# Function to comment out library and package installation statements in R code
# Arguments:
#   code: R code as a character vector or string
#   package_name: Name of the package to be commented out
# Returns:
#   A modified character vector with library and package installation statements 
# commented out

comment_out_library_statements <- function(code, package_name) {
  # Replace occurrences of "if (!require(Package_name))" with the alternate 
  # package
  code_with_if_statement <- gsub(paste0("if \\(\\!require\\(", package_name, 
                                        "\\)\\)"), "if (FALSE)", code)
  
  # Use regular expressions to find library and package installation statements
  lib_pattern <-
    paste0("\\b",
           "library",
           "\\s*\\(\\s*",
           package_name,
           "\\s*\\)",
           "\\b")
  
  install_pattern <- paste0(
    "\\b(?:library|install\\.packages|BiocManager::install|install_github)\\s*\\(\\s*[\"]",
    package_name,
    "[\"]\\s*\\)",
    "\\b"
  )
  
  # Combine both patterns using the OR operator (|) and comment out the lines
  code_with_comments <-
    gsub(paste(lib_pattern, install_pattern, sep = "|"),
         "# \\0",
         code_with_if_statement)
  
  return(code_with_comments)
}



# Function to run external R code with specified package suppression and cleanup
# Arguments:
#   code: R code as a character vector or string
#   package_to_suppress: Name of the package to suppress during code execution
#   packages_used: Names of packages used in the code
# Returns:
#   None

run_external_r_code <- function(code, package_to_suppress,
                                packages_used) {
  # Comment out library loading statements for the specified package
  code_content <-
    comment_out_library_statements(code, package_to_suppress)
  
  # Create a new environment for running the external code
  external_env <- new.env()
  
  # Unload all the packages to ensure they are fully removed from the R session
  for (pkg in packages_used) {
    detachPackage(pkg)
  }
  
  # Execute the code within the new environment
  with(external_env, {
    # Evaluate the external code
    eval(parse(text = paste(code_content, collapse = "\n")))
  })
  
  # Release memory by closing the new environment
  rm(list = ls(envir = external_env), envir = external_env)
}


###-------Process code----------------------------------------------------------

# Read the content of the original R file
code <- readLines(Path_to_code)

# Detect packages present in the code
packages_used <- detect_packages_in_code(code)

# Create variables for each Package
for (pkg in packages_used) {
  var_name <- paste0("Package_", pkg)
  var_value <- NULL
  assign(var_name, var_value)
}

for (pkg in packages_used) {
  cat("\n Testing without package: ", pkg, "\n")
  
  var_name <- paste0("Package_", pkg)
  
  # Initialize local variables for result and timeout_flag
  local_result <- NULL
  timeout_flag <- FALSE
  
  # Wrap the code execution in tryCatch and withTimeout
  tryCatch({
    withTimeout({
      # Call the function to run the external R code with library suppression
      local_result <<- run_external_r_code(code, pkg, packages_used)
      
      local_result <<- "Success"
    }, timeout = time_for_code_to_run)
  }, timeout = function(e) {
    # If a timeout occurs, set the flag and local_result
    timeout_flag <<- TRUE
    local_result <<- "Timeout"
  }, error = function(e) {
    if (grepl(
      "reached elapsed time limit|evaluation nested too deeply|evaluation error|reached CPU time limit",
      e$message,
      ignore.case = TRUE
    )) {
      # Handle the specific timeout error
      timeout_flag <- TRUE
      local_result <<- "Timeout"
    } else {
      # Handle other errors
      local_result <<- "Error"
    }
  })
  
  # If the timeout flag is set, update the local_result
  if (timeout_flag) {
    local_result <<- "Timeout"
  }
  
  assign(var_name, local_result)
}


###-------Indicate completion---------------------------------------------------

# Check if each dynamically created variable is FALSE or TRUE
for (pkg in packages_used) {
  var_name <- paste0("Package_", pkg)
  var_value <- get(var_name)
  
  if (var_value == "Error") {
    print(paste(var_name, "is being used and cannot be deleted."))
  }
  else if (var_value == "Success") {
    print(paste(var_name, "can be deleted."))
  }
  else {
    print(paste(var_name, "can be deleted."))
  }
  
}



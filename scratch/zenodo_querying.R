query_zenodo_safe <- function(url, timeout_sec = 60, max_retries = 3) {
  for (attempt in 1:max_retries) {
    result <- tryCatch(
      {
        response <- GET(url, timeout(timeout_sec))

        if (status_code(response) == 200) {
          return(list(success = TRUE, content = content(response, as = "parsed")))
        } else {
          list(success = FALSE, error = sprintf("HTTP %d", status_code(response)))
        }
      },
      error = function(e) {
        list(success = FALSE, error = as.character(e$message))
      }
    )

    if (result$success) {
      return(result$content)
    }

    # Log the error
    message(sprintf("  Attempt %d/%d failed: %s", attempt, max_retries, result$error))

    # Wait before retry (exponential backoff)
    if (attempt < max_retries) {
      wait_time <- 2^attempt
      message(sprintf("  Waiting %d seconds before retry...", wait_time))
      Sys.sleep(wait_time)
    }
  }

  stop(sprintf("Failed after %d attempts. Last error: %s", max_retries, result$error))
}

# New approach: Search with a smaller page size and longer timeout
find_version_robust <- function(
    target_date,
    concept_doi = "10.5281/zenodo.5519056") {
  target <- as.Date(target_date)
  concept_recid <- gsub("10.5281/zenodo.", "", concept_doi)

  message(sprintf("Searching for version on or before %s...", target_date))

  base_url <- "https://zenodo.org/api/records"

  # Try with smaller page size and longer timeout
  page <- 1
  max_pages <- 3 # Limit to 3 pages (300 records) to avoid timeout
  page_size <- 100

  while (page <= max_pages) {
    message(sprintf("  Checking page %d (timeout: 60s)...", page))

    url <- modify_url(
      base_url,
      query = list(
        q = sprintf('conceptrecid:"%s"', concept_recid),
        all_versions = "true",
        sort = "mostrecent",
        page = page,
        size = page_size
      )
    )

    # Use longer timeout and retry logic
    content <- tryCatch(
      {
        query_zenodo_safe(url, timeout_sec = 60)
      },
      error = function(e) {
        message(sprintf("  Failed to fetch page %d: %s", page, e$message))
        return(NULL)
      }
    )

    if (is.null(content) || length(content$hits$hits) == 0) {
      message("  No more results or timeout")
      break
    }

    # Extract versions
    versions <- lapply(content$hits$hits, function(x) {
      list(
        record_id = x$id,
        doi = x$doi,
        publication_date = as.Date(x$metadata$publication_date),
        title = x$metadata$title
      )
    })

    versions_df <- bind_rows(versions)

    message(sprintf(
      "  Retrieved %d versions (dates: %s to %s)",
      nrow(versions_df),
      min(versions_df$publication_date),
      max(versions_df$publication_date)
    ))

    # Check if we found a match
    eligible <- versions_df %>%
      filter(publication_date <= target) %>%
      arrange(desc(publication_date))

    if (nrow(eligible) > 0) {
      closest <- eligible[1, ]
      message(sprintf(
        "✓ Found version from %s (DOI: %s)",
        closest$publication_date, closest$doi
      ))
      return(closest)
    }

    # Check if we've gone past our target
    oldest_on_page <- min(versions_df$publication_date)
    if (oldest_on_page > target) {
      message(sprintf(
        "  Need to check older versions (oldest so far: %s)",
        oldest_on_page
      ))
      page <- page + 1
    } else {
      message(sprintf("  Searched back to %s, no match found", oldest_on_page))
      break
    }
  }

  stop(sprintf(
    "Could not find version on or before %s within %d pages (%d records). Try a more recent date.",
    target_date, max_pages, max_pages * page_size
  ))
}

# Alternative: Use a known record ID if you have one
get_zenodo_record_by_id <- function(record_id) {
  message(sprintf("Fetching record %s...", record_id))

  url <- sprintf("https://zenodo.org/api/records/%s", record_id)
  content <- query_zenodo_safe(url, timeout_sec = 30)

  record_info <- list(
    record_id = content$id,
    doi = content$doi,
    publication_date = as.Date(content$metadata$publication_date),
    title = content$metadata$title
  )

  return(record_info)
}

# Download file with better error handling
download_zenodo_file <- function(record_id, file_name, dest_path = NULL) {
  url <- sprintf("https://zenodo.org/api/records/%s", record_id)

  message(sprintf("Getting file list for record %s...", record_id))
  content <- query_zenodo_safe(url, timeout_sec = 30)

  # Find the file
  files <- content$files
  file_match <- NULL

  for (f in files) {
    if (grepl(file_name, f$key, fixed = TRUE)) {
      file_match <- f
      break
    }
  }

  if (is.null(file_match)) {
    available_files <- sapply(files, function(x) x$key)
    stop(sprintf(
      "File '%s' not found.\nAvailable files:\n  %s",
      file_name,
      paste(available_files, collapse = "\n  ")
    ))
  }

  # Download the file
  download_url <- file_match$links$self

  if (is.null(dest_path)) {
    dest_path <- file.path(tempdir(), file_match$key)
  }

  message(sprintf(
    "Downloading %s (%.2f MB)...",
    file_match$key,
    file_match$size / 1024^2
  ))

  # Download with longer timeout
  download_result <- tryCatch(
    {
      GET(
        download_url,
        write_disk(dest_path, overwrite = TRUE),
        progress(),
        timeout(300) # 5 minute timeout for download
      )
      TRUE
    },
    error = function(e) {
      message(sprintf("Download failed: %s", e$message))
      FALSE
    }
  )

  if (!download_result) {
    stop("Failed to download file")
  }

  message(sprintf("✓ Downloaded to: %s", dest_path))

  return(dest_path)
}

# Main function with better error messages
get_rki_zenodo_data <- function(
    target_date,
    concept_doi = "10.5281/zenodo.5519056",
    file_name = "Aktuell_Deutschland_adjustierte-COVID-19-Hospitalisierungen.csv",
    return_path_only = FALSE) {
  message(sprintf("\n=== Getting RKI data as of %s ===\n", target_date))

  # Find the version
  version <- tryCatch(
    {
      find_version_robust(target_date, concept_doi)
    },
    error = function(e) {
      message("\n Failed to find version automatically")
      message("This might be due to:")
      message("  1. Network/firewall issues blocking Zenodo")
      message("  2. Zenodo API being slow/overloaded")
      message("  3. Date too far back (data only available from ~2021)")
      message("\nTrying alternative approach...")

      # Suggest manual approach
      stop(sprintf(
        "Could not find version for %s.\n\nTo work around this:\n  1. Visit https://zenodo.org/record/8088569\n  2. Click 'Versions' to browse all versions\n  3. Find the DOI for your target date\n  4. Use: get_zenodo_data_by_doi('10.5281/zenodo.XXXXXX')",
        target_date
      ))
    }
  )

  # Download the file
  file_path <- download_zenodo_file(version$record_id, file_name)

  if (return_path_only) {
    return(file_path)
  }

  # Read the CSV
  message("Reading CSV file...")
  data <- read.csv(file_path, stringsAsFactors = FALSE)

  # Add metadata
  attr(data, "zenodo_doi") <- version$doi
  attr(data, "zenodo_date") <- version$publication_date
  attr(data, "target_date") <- target_date

  message(sprintf("✓ Loaded %d rows\n", nrow(data)))

  return(data)
}

# New function: Get data if you know the specific DOI
get_zenodo_data_by_doi <- function(
    doi,
    file_name = "Aktuell_Deutschland_adjustierte-COVID-19-Hospitalisierungen.csv",
    return_path_only = FALSE) {
  # Extract record ID from DOI
  record_id <- gsub(".*zenodo.", "", doi)

  message(sprintf("\n=== Getting data from DOI: %s ===\n", doi))

  # Get record info
  version <- get_zenodo_record_by_id(record_id)

  # Download the file
  file_path <- download_zenodo_file(record_id, file_name)

  if (return_path_only) {
    return(file_path)
  }

  # Read the CSV
  message("Reading CSV file...")
  data <- read.csv(file_path, stringsAsFactors = FALSE)

  # Add metadata
  attr(data, "zenodo_doi") <- version$doi
  attr(data, "zenodo_date") <- version$publication_date

  message(sprintf("✓ Loaded %d rows\n", nrow(data)))

  return(data)
}

# Function to list recent versions only (safer)
list_recent_versions <- function(n = 50, concept_doi = "10.5281/zenodo.5519056") {
  concept_recid <- gsub("10.5281/zenodo.", "", concept_doi)

  message(sprintf("Fetching %d most recent versions...", n))

  url <- modify_url(
    "https://zenodo.org/api/records",
    query = list(
      q = sprintf('conceptrecid:"%s"', concept_recid),
      all_versions = "true",
      sort = "mostrecent",
      size = min(n, 100)
    )
  )

  content <- query_zenodo_safe(url, timeout_sec = 60)

  versions <- lapply(content$hits$hits, function(x) {
    list(
      record_id = x$id,
      doi = x$doi,
      publication_date = as.Date(x$metadata$publication_date),
      title = x$metadata$title
    )
  })

  versions_df <- bind_rows(versions)

  message(sprintf("✓ Retrieved %d versions\n", nrow(versions_df)))

  return(versions_df)
}

data <- get_rki_zenodo_data("2024-09-30")
test <- get_zenodo_data_by_doi("10.5281/zenodo.17490610")

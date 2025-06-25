# -------------------------------
# R Script: ArcGIS Geoprocessing of Aerial Survey Data via R
# Name: Jonah Bacon
# Email: jonah.bacon@alaska.gov
# Date: 25 June 2025

# Requirements:
#   • ArcGIS (for the geoprocessing environment)
#   • arcgisbinding package installed and configured
#   • sf, dplyr, and writexl packages installed

# This script takes a CSV input file containing salmon escapement count survey data by location.
# It outputs two shapefiles (unjoined points and spatially joined points) and a summary text file and Excel workbook.

# Notes:
#  • This script automates data import, spatial join, and output file generation.
#  • This script does include manual editing steps in the beginning (i.e., updating naming conventions to match current survey)
#    and at the end (i.e., using ArcGIS to move observation locations that have Join_Count == 0 into the proper stream section buffer zone).
#    Instances where Join_Count == 0 are flagged for review in the 'failed.join_sf' dataframe. If observations exist in this data frame,
#    the input csv must be updated.

# -------------------------------

# Load required libraries
library(arcgisbinding)  # Bridge to ArcGIS (initializes connection)
library(sf)            # For spatial handling
library(dplyr)         # For data transformation
library(writexl)       # For saving summary file as Excel workbook

# Verify ArcGIS connection and license status
arc.check_product()

# --------------------------------------------------------------------
# Define Input and Output Paths
# --------------------------------------------------------------------

### NEED MODIFICATION OR VERIFICATION THAT VALUES ARE CORRECT:

# Define current year (update as needed)
currentyear <- 2025

# Define input directory and file
input_rawfile_dir <- paste0("O:/DCF/SALMON/ESCAPEMENT/", currentyear, "/AERIAL/1_RAW/counts")
input_csv_name <- list.files(input_rawfile_dir)[length(list.files(input_rawfile_dir))] # Choose the most recently updated aerial survey (at the end of the directory list)

# Verify that 'input_csv_name' is the correct file you wish to analyze
print(input_csv_name)

# Define naming convention parameters
# YOU MUST MODIFY DISTRICT AND SURVEYOR TO MATCH THE INPUT SURVEY EACH TIME THE SCRIPT IS RUN
date_str  <- regmatches(input_csv_name, gregexpr("2025....", input_csv_name))[[1]] # Verify this is correct
district  <- "TEST"         # Manually change for each survey
surveyor  <- "Hollowell"    # Manually change if surveyor was not Glenn Hollowell


### ONCE ABOVE VALUES ARE VERIFIED CORRECT, THE REMAINDER OF THE SCRIPT DOES NOT NEED ANY MODIFICATION:

# Define output directories
output_unjoined_dir <- paste0("O:/DCF/SALMON/ESCAPEMENT/", currentyear, "/AERIAL/4_GIS/5_UNJOINED")
output_joined_dir   <- paste0("O:/DCF/SALMON/ESCAPEMENT/", currentyear, "/AERIAL/4_GIS/6_JOINED")
output_summary_txt_dir  <- paste0("O:/DCF/SALMON/ESCAPEMENT/", currentyear, "/AERIAL/4_GIS/9_SUMMARIES/TEXT")
output_summary_xlsx_dir  <- paste0("O:/DCF/SALMON/ESCAPEMENT/", currentyear, "/AERIAL/4_GIS/9_SUMMARIES/EXCEL")

# Define the path to the buffer polygon shapefile (This is the LCI_POLYGONS.shp)
polygon_shp <- "O:/DCF/SALMON/ESCAPEMENT/ARC_MAP_FILES/AERIAL_BUFFERS/Aerial_Buffers_SALMON/LCI_POLYGONS.shp"

# Create output filenames based on naming convention
unjoined_shp_name <- paste0(district, "_", date_str, "_", surveyor, "_UNJOINED.shp")
joined_shp_name   <- paste0(district, "_", date_str, "_", surveyor, "_JOINED.shp")
summary_txt_name      <- paste0(district, "_", date_str, "_", surveyor, "_Summary.txt")
summary_xlsx_name      <- paste0(district, "_", date_str, "_", surveyor, "_Summary.xlsx")

# --------------------------------------------------------------------
# Step 1: Read the Input CSV and Create a Point Feature Class
# --------------------------------------------------------------------

# Read CSV
df <- read.csv(paste0(input_rawfile_dir,"/",input_csv_name), header = FALSE) %>% 
  select(-1) %>%                      # Select all columns except the first column (which is just the row number)
  setNames(as.character(.[1, ])) %>%  # Rename columns using row 1
  slice(-1) %>%                       # Remove the first row
  select(-"NA")                       # Remove column with name "NA" if it exists

# Create an sf object (point geometry) from coordinate columns (WGS84)
points_sf <- st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  rename_with(~ substr(., 1, 10))  # Rename columns to only include first 10 characters to comply with .dbf file requirements

# Save the unjoined points as a shapefile
output_unjoined_path <- file.path(output_unjoined_dir, unjoined_shp_name)
st_write(points_sf, output_unjoined_path, quiet = TRUE, delete_layer = TRUE)
cat("Unjoined points shapefile created at:", output_unjoined_path, "\n")

# --------------------------------------------------------------------
# Step 2: Read the Buffer Polygon (Join Features) Shapefile
# --------------------------------------------------------------------

polygons_sf <- st_read(polygon_shp)

# Transform polygons to the same CRS as points if needed
if (st_crs(points_sf) != st_crs(polygons_sf)) {
  polygons_sf <- st_transform(polygons_sf, st_crs(points_sf))
}

# Clean the polygon geometry by rebuilding them to valid shapes
# Removes self-intersections/repeated coordinates
polygons_sf <- st_make_valid(polygons_sf)

# --------------------------------------------------------------------
# Step 3: Perform the Spatial Join (Point-in-Polygon)
# --------------------------------------------------------------------

# Spatially join polygon attributes onto the points
# Here we use st_join() with st_intersects. Each point from the survey that falls inside a stream section
# buffer polygon will inherit the polygon’s attributes.
joined_sf <- st_join(points_sf, polygons_sf, join = st_intersects, left = TRUE) %>% 
  mutate(Join_Count = ifelse(is.na(STOCK), 0, 1)) %>% # Create column for instances where spatial join failed
  rename_with(~ substr(., 1, 10))  # Rename columns to only include first 10 characters to comply with .dbf file requirements

# --------------------------------------------------------------------
# Step 4: Manual QA/QC Edit of Survey Data Location (if necessary)
# --------------------------------------------------------------------

# Filter the joined_sf by instances where survey count locations did not occur within a stream section buffer polygon:
failed.join_sf <- filter(joined_sf, Join_Count == 0)

# If observations exist in the 'failed.join_sf' data frame, you must manually edit the input csv 
# ('input_csv_name'). Find the row(s) corresponding to the failed spatial join. Manually adjust the 'Latitude'
# and 'Longitude' column values for the row(s) so that the survey observation is located within the correct stream
# section buffer geometry. Then save the csv file. Rerun the script to this point and verify that the 'failed.join_sf'
# data frame is empty. If so, then proceed. If not, repeat this step.

# Save the joined points shapefile
output_joined_path <- file.path(output_joined_dir, joined_shp_name)
st_write(joined_sf, output_joined_path, delete_layer = TRUE)
cat("Joined shapefile created at:", output_joined_path, "\n")

# --------------------------------------------------------------------
# Step 5: Create a Summary Table from Joined Data
# --------------------------------------------------------------------

# Aggregating survey counts by stock, species, and stream section:
#   • "Surveycount" is a field holding numeric survey counts of salmon,
#   • "Quality" holds an integer value corresponding to the quality rating of the survey (1 = Perfect, 5 = Worst), and
#   • "DeviceTime" holds a time value corresponding to when the survey observation occurred.
summary_tbl <- joined_sf %>%
     st_set_geometry(NULL) %>%                               # Drop spatial geometry
     group_by(STOCK, Species, Sect_Code, SamplerName) %>%    # Group by variables of interest
     summarise(                                              # Summarise
         FREQUENCY = n(),
         SUM_COUNT = sum(Surveycount, na.rm = TRUE),
         MEAN_OVERALL_RATING = mean(Quality, na.rm = TRUE),
         FIRST_DEVICE_TIME = min(DeviceTime, na.rm = TRUE),
         .groups = "drop"
     ) %>% 
  rename(                                                   # Rename columns to match column names from previous years
    SPECIES = Species,
    SECT_CODE = Sect_Code,
    SAMPLERNAME = SamplerName
  )

# Write the summary as a tab-delimited text file
summary_txt_output_path <- file.path(output_summary_txt_dir, summary_txt_name)
write.table(summary_tbl, summary_txt_output_path, sep = "\t", row.names = FALSE, quote = FALSE)
cat("Text file summary table created at:", summary_txt_output_path, "\n")

# Write the summary as an Excel workbook file
summary_xlsx_output_path <- file.path(output_summary_xlsx_dir, summary_xlsx_name)
write_xlsx(summary_tbl, path = summary_xlsx_output_path, format_headers = FALSE)
cat("Excel workbook summary table created at:", summary_xlsx_output_path, "\n")

# --------------------------------------------------------------------
# End of Script
# --------------------------------------------------------------------

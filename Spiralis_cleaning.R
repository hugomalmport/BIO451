#Clear the workspace
rm(list = ls())
# Load the library
library(readxl)

#Read the excel file from its path
##Make sure to change path depending on where you saved it!
###Change sheet depending on which sheet is going to be used!! - so either "Data" or "Salinity Sampling" 
spiralis_data_raw <- read_excel("~/Downloads/Fucus Data-8.xlsx", sheet = "Salinity Sampling")

#Inspect the file
summary(spiralis_data_raw)
head(spiralis_data_raw)
str(spiralis_data_raw)


# Remove superfluous columns that did not have a name in Excel
spiralis_data_clean <- spiralis_data_raw %>% select(-starts_with("..."))
str(spiralis_data_clean)

# Remove rows with NA values in field "powder_mg"
spiralis_data_clean <- spiralis_data_clean |> drop_na("powder_mg")



# Replace "/" with "_" in column names
colnames(spiralis_data_clean) <- colnames(spiralis_data_clean) |> str_replace_all("/", "_")

# Make life_stage a factor
spiralis_data_clean$life_stage <- as.factor(spiralis_data_clean$life_stage)


# Add columns for the photosynthetic pigments and calculate concentrations
spiralis_data_clean <- mutate(spiralis_data_clean, chlorophyll_a = (0.6 * (11.47 * p_664nm - 0.40 * p_630nm)) / powder_mg)
spiralis_data_clean <- mutate(spiralis_data_clean, chlorophyll_c = (0.6 * (24.36 * p_630nm - 3.73 * p_664nm)) / powder_mg)
spiralis_data_clean <- mutate(spiralis_data_clean, carotenoids = (0.6 * (7.6 * p_480nm - 1.49 * p_510nm)) / powder_mg)

# Convert column "sample_num" to integer
spiralis_data_clean$sample_num <- as.integer(spiralis_data_clean$sample_num)

# Split data for the different analyses 
spiralis_data_no_touch <- subset(spiralis_data_clean, (sample_num > 360 & sample_num <= 386))
spiralis_data_wild <- subset(spiralis_data_clean, treatment_psu == 27)
spiralis_data_clean <- subset(spiralis_data_clean, sample_num < 361)

spiralis_data_clean <- spiralis_data_clean |>
  mutate(desiccation = case_when(sample_num < 181 ~ 'N',
                                 sample_num > 180  ~ 'Y')
  )

# Make the salinity treatment a factor
spiralis_data_no_touch$treatment_psu <- as.factor(spiralis_data_no_touch$treatment_psu)
spiralis_data_wild$treatment_psu <- as.factor(spiralis_data_wild$treatment_psu)
spiralis_data_clean$treatment_psu <- as.factor(spiralis_data_clean$treatment_psu)

# Convert column "sample_num" to integer
spiralis_data_clean$sample_num <- as.integer(spiralis_data_clean$sample_num)

# Check that the levels for the salinity treatment are correct
levels(spiralis_data_clean$treatment_psu)

# Check that the levels life_stage are correct
levels(spiralis_data_clean$life_stage)

##Check the data frames
print(spiralis_data_no_touch)
print(spiralis_data_no_touch)
print(spiralis_data_clean)

library(dataMaid)
library(labelled)
attr(spiralis_data_clean$sample_num, "shortDescription") <- "Each sample received a unique sample number"
attr(spiralis_data_clean$life_stage, "shortDescription") <- "The two life stages in the experiment. Adult (A) and germling (G)."
attr(spiralis_data_clean$position, "shortDescription") <- "Relative postion in location (site) groups. To be used to identify samples from the same adult. Were in the end never used."
attr(spiralis_data_clean$treatment_psu, "shortDescription") <- "The salinity the sample was treated in"
attr(spiralis_data_clean$location, "shortDescription") <- "The site were the sample was collected."
attr(spiralis_data_clean$tank, "shortDescription") <- "The number of the tank in the experiment"
attr(spiralis_data_clean$empty_epp_mg, "shortDescription") <- "The weight of the empty eppendorf tube for the sample"
attr(spiralis_data_clean$epp_powder, "shortDescription") <- "The weight of the eppendorf tube with the powderized sample"
attr(spiralis_data_clean$powder_mg, "shortDescription") <- "The weight of the powderized sample in mg"
attr(spiralis_data_clean$p_630nm, "shortDescription") <- "Absorbance value at wavelength 630 nm"
attr(spiralis_data_clean$p_664nm, "shortDescription") <- "Absorbance value at wavelength 664 nm"
attr(spiralis_data_clean$p_480nm, "shortDescription") <- "Absorbance value at wavelength 480 nm"
attr(spiralis_data_clean$p_510nm, "shortDescription") <- "Absorbance value at wavelength 510 nm"
attr(spiralis_data_clean$p_630nm_mg, "shortDescription") <- "Absorbance value at wavelength 630 nm normalized by dividing with powder weight"
attr(spiralis_data_clean$p_664nm_mg, "shortDescription") <- "Absorbance value at wavelength 664 nm normalized by dividing with powder weight"
attr(spiralis_data_clean$p_480nm_mg, "shortDescription") <- "Absorbance value at wavelength 480 nm normalized by dividing with powder weight"
attr(spiralis_data_clean$p_510nm_mg, "shortDescription") <- "Absorbance value at wavelength 510 nm normalized by dividing with powder weight"
attr(spiralis_data_clean$chlorophyll_a, "shortDescription") <- "Calculated chlorophyll a concentration (mg/g) in the sample"
attr(spiralis_data_clean$chlorophyll_c, "shortDescription") <- "Calculated chlorophyll c concentration (mg/g) in the sample"
attr(spiralis_data_clean$carotenoids, "shortDescription") <- "Calculated carotenoid concentration (mg/g) in the sample"
attr(spiralis_data_clean$desiccation, "shortDescription") <- "Flag to indicate if the sample been subject to desiccation or not. Y = Yes, sample has been desiccated. N = No, sample has not been desiccated."

makeCodebook(spiralis_data_clean, replace=TRUE)

#Decide the file name and path
file_path <- "~/Downloads/cleaned_data.Rdata"

# Write the dataframes to a Rdata file 
save(spiralis_data_clean, spiralis_data_no_touch, spiralis_data_wild, file = file_path)



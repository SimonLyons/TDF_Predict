

# My following along on Hilary Parker's guide to creating packages
# https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/

# Step 0: Load relevant packages
install.packages("devtools")
install.packages("roxygen2")
library(devtools)
library(roxygen2)

# Create working directory
setwd(dir = "C:/b_Data_Analysis/Projects/TDF_Predict/TDF_Predict/")
# Step 1: Create a directory with the bare minimum folders of R packages
create("rouleur")

# Step 2: Add functions (I've just created a new R file with a copy of Hilary's 'cat' function)

# Step 3: Add documentation
# I've added comments at the start of the cat function in the format suggested by Hilary

# Step 4: Process your documentation
# Now you need to create the documentation from your annotations earlier.
# Set working directory to the location of the function(s)
setwd(dir = "C:/b_Data_Analysis/Projects/TDF_Predict/TDF_Predict/rouleur/")
document()
# This automatically adds in the .Rd files to the man directory, and adds a NAMESPACE file to the main directory. 
# SIMON: This isn't adding the .Rd files to the 'man' directory.

# Step 5: Install!
# Now it is as simple as installing the package! You need to run this from the parent working directory that contains the cats folder.
setwd("..")
getwd()
install("rouleur")


# (Bonus) Step 6: Make the package a GitHub repo
# This isn’t a post about learning to use git and GitHub — for that I recommend Karl Broman’s Git/GitHub Guide. 
# SIMON: This isn't working yet. 
# I get 'Error in stop(github_error(request)) : Not Found (404)'
install_github("rouleur","SimonLyons/rouleur")


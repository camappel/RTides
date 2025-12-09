import zipfile
import os

# Path to the zip file
zip_path = 'data/GESLA4_ALL.zip'
# Directory to extract to
extract_dir = 'data/GESLA4_ALL'

# Create the extraction directory if it doesn't exist
if not os.path.exists(extract_dir):
    os.makedirs(extract_dir)

# Extract the zip file
with zipfile.ZipFile(zip_path, 'r') as zip_ref:
    zip_ref.extractall(extract_dir)

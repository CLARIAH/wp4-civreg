# slices image in half vertically (two equal sides)
# necessary to select single pages for Leiden Hoofdelijke Omslag tax

import cv2
import glob
import os.path

path = "*.jpg"

files = glob.glob("*.jpg")
print(files)

# Read the image
for file in glob.glob(path):
    img = cv2.imread(file)
    print(img.shape)
    height = img.shape[0]
    width = img.shape[1]
# Cut the image in half
    width_cutoff = width // 2
    left = img[:, :width_cutoff]
    right = img[:, width_cutoff:]

#save image (left)
    basename = os.path.basename(file)# e.g. MyPhoto.jpg
    name = os.path.splitext(basename)[0]  # e.g. MyPhoto
    cv2.imwrite(name + '_left.jpg', left)

#save image (right)
    basename = os.path.basename(file)  # e.g. MyPhoto.jpg
    name = os.path.splitext(basename)[0]  # e.g. MyPhoto
    cv2.imwrite(name + '_right.jpg', right)

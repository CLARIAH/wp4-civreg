# crops image to relevant text areo only for Leiden income tax, left-hand pages
# TODO: make 1 python loop for left and right

import cv2
import glob
import os
import numpy as np

# Get list of filenames to convert
files = glob.glob("*_left")
print(files)

import cv2
import glob
import os.path

path = "*_left.tiff"

for file in glob.glob(path):

# left:
    image = cv2.imread(file)


    y=300 # left
    x=180 # top
    h=1700 # right
    w=2100 # bottom

    crop_image = image[x:w, y:h]

    basename = os.path.basename(file)  # e.g. MyPhoto.jpg
    name = os.path.splitext(basename)[0]  # e.g. MyPhoto
    print(name)
    cv2.imwrite(name + "_cropped.tiff", crop_image)

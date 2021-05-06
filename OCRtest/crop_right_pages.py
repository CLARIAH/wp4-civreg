# crops image to relevant text areo only for Leiden income tax, right-hand pages
# TODO: make 1 python loop for left and right



import cv2
import glob
import os
import numpy as np

# Get list of filenames to convert
files = glob.glob("*_right")
print(files)

import cv2
import glob
import os.path

path = "*_right.jpg"

for file in glob.glob(path):

# left:
    image = cv2.imread(file)


    y=150 # left
    x=180 # top
    h=1450 # right
    w=2100 # bottom

    crop_image = image[x:w, y:h]

    basename = os.path.basename(file)  # e.g. MyPhoto.jpg
    name = os.path.splitext(basename)[0]  # e.g. MyPhoto
    print(name)
    cv2.imwrite(name + "_cropped.png", crop_image)

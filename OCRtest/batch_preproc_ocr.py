# script to OCR printed digitized Hoofdelijke Omslag tax Leiden
# see https://www.erfgoedleiden.nl/collecties/archieven/archievenoverzicht/details/NL-LdnRAL-1698
# step 1: cut image vertically (py script)
# step 2: crop text area (py script)


import cv2
import glob
import os
import numpy as np
import cv2
import pytesseract
from pytesseract import image_to_string
import os.path

pytesseract.pytesseract.tesseract_cmd = 'C:\\Program Files\\Tesseract-OCR\\tesseract.exe'

# Get list of filenames to convert
files = glob.glob("*.png")
print(files)

path = "*.png"

for file in glob.glob(path):
    img = cv2.imread(file)

# rescale the image.
    img = cv2.resize(img, None, fx=1.4, fy=1.4, interpolation=cv2.INTER_CUBIC) # 1.4,1.4

# remove noise
    img = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)

    kernel = np.ones((1, 1), np.uint8)
    img = cv2.dilate(img, kernel, iterations=2)
    img = cv2.erode(img, kernel, iterations=2)
    img = cv2.GaussianBlur(img, (5,5), 0) # 5,5 works
    img = cv2.medianBlur(img,5)

# binarization
    BINARY_THREHOLD = 180 # 180
    img = cv2.threshold(img, BINARY_THREHOLD, 255, cv2.THRESH_BINARY + cv2.THRESH_OTSU)[1]

# increase contrast
    alpha=1.5 # 1.5 works
    beta=20 # 20 works

    img= cv2.addWeighted(img,alpha,np.zeros(img.shape, img.dtype),0,beta)

# save processed image
    basename = os.path.basename(file)
    name = os.path.splitext(basename)[0]
    cv2.imwrite(name + '_processed.jpg', img)

# recognize text with tesseract for python
    #ocr = pytesseract.image_to_string(img)
    ocr = pytesseract.image_to_string(img, config="-c tessedit_char_whitelist=01234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.- --psm 6")

# save ocr as text
    with open(name + '_processed.txt', 'w') as f:
        f.write(ocr)

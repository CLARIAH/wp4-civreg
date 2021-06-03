import glob
from PIL import Image
import os

images = glob.glob("C:\\Users\\Ruben\\Documents\\02. Werk\\Clariah\\Pilots\\RAL 1698 inv. 13\\*.jp2")
for image in images:
    with open(image, "rb") as file:
        img = Image.open(file)
        name = os.path.splitext(image)[0] + '.tiff'
        img.save(name, 'tiff')

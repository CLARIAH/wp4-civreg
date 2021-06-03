# OCR Leiden tax registers

Test pipeline to prepare, process, and OCR images from the Hoofdelijke Omslag Leiden (income tax) 1877-1921. 

The source can be found here [https://www.erfgoedleiden.nl/collecties/archieven/archievenoverzicht/details/NL-LdnRAL-1698](https://www.erfgoedleiden.nl/collecties/archieven/archievenoverzicht/details/NL-LdnRAL-1698)

### Instruction (in progress)
1. download .jpg files from [URL](https://www.erfgoedleiden.nl/collecties/archieven/archievenoverzicht/details/NL-LdnRAL-1698) (manually for now)
2. optional: convert image to jpeg/png using `image_convert.py`
3. `slice_image.py` to cut image vertically
4. delete empty pages (manually, depending on tax year)
5. `crop_left/right_pages.py` to create images with text only (no borders and black edges)  **TODO: crop both in one script**
6. `batch_preproc_ocr.py` to batch OCR all images. This will create a processed `.png` file and a corresponding `.txt` file with the OCRed text for each image.

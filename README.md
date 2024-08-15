# USF Library Ogden Collection resources

Repository to contain annotation workflow code, basic databases such as controlled vocabulary, and perhaps draft workflows, instructions, or data presentation products (like R Shiny apps) - any of which might later be developed into libguides or use other USF Library-specific resources.

If you're new to GitHub, coding, or data management, feel free to look at the resources I've collected in my lab's general repository: https://github.com/McMinds-Lab/analysis_templates

Existing controlled vocabulary file is just a start. 
- Each row in that file would correspond to a column in the final product.
- The first column of the vocab file has the names of the columns in the final product
- The second column of the vocab file has the controlled vocabulary allowed for that annotation (or keywords signifying that the annotation involves coordinate selection within the photo)
- The third column of the vocab file has the exact prompt that will be displayed in the GUI
- The fourth column of the vocab file has 'dependencies' - these are used to tell the gui to skip the question if it is irrelevant (e.g. there's no point asking what the coral species are in the photo, if we have already determined that there are no corals at all)
- Ideally, we should be able to find wider standards for controlled vocabulary rather than invent our own (e.g. https://github.com/gbif/vocabulary)

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

The custom photo annotator can be used by a couple methods. It might not be as good as some polished software, but is nicely customizeable - it's worth looking at various other tools and maybe comparing this to them. To play around with it, there are example 'vocabulary' files and photos included. The easiest are:
1. Use the version that's been exported to https://thecnidaegritty.org/photo_annotator . This won't necessarily be up-to-date with the code maintained here (and currently has a bug for annotations of bounding boxes or point coordinates)
2. If we make this repo public, copying and pasting the command `shiny::runGitHub(repo = 'McMinds-Lab/USF_Library_Ogden_Collection_resources', ref = 'main', subdir = 'shiny_annotation')` in an R interpreter with the Shiny package installed should work.
3. Download this repo (download the .zip file with the big green `code` button or use `git clone git@github.com:McMinds-Lab/USF_Library_Ogden_Collection_resources.git` in a terminal) and then use the command `shiny::runApp('path/to/USF_Library_Ogden_Collection_resources/photo_annotator')`

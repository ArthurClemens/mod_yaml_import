# YAML data import module for Zotonic

This module let's you import data in [YAML](http://en.wikipedia.org/wiki/YAML) format into Zotonic, to automatically create Pages from the YAML data.

It includes interface options to:

* rename fields
* select fields to exclude
* map the title field if it does not exist in the data
* map the fields to other data types (for instance to import media files, or to use a predicate)
* preview data from the YAML file
* import a range

Next to creating Pages you can also create Category Pages.


## Installation

See file INSTALL


## One category at a time

The data file cannot have more than one data structure: all records should be structured the same. Split up the YAML file if you need to import pages for multiple categories.


## Troubleshooting

* In case international characters are garbled: make sure the text is saved as UTF-8 (no BOM).
* In case the module does not import the file at all: make sure the file ends with:

        ...

    plus a newline at the bottom.
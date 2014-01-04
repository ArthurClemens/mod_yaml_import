# YAML data import module for Zotonic

This module lets you import data in [YAML](http://en.wikipedia.org/wiki/YAML) format to create new Zotonic Pages.

Pages are represented as YAML list items. Item properties are translated to Page attributes (title, summary, is_published, ...).

Medium URLs can be attached automatically; pages can be connected using predicates.

Next to pages, categories and predicates can be created as well.

---

**Before trying out, first backup you data!**

---

## Example data file


    %YAML 1.1
    ---
    -
      title: "Cheesy Vegemite pull-apart"
      summary: "Put these cheesy vegemite scrolls into the school lunch-box as an exciting alternative to the sandwich."
      img: "http://www.taste.com.au/images/recipes/sfi/2009/03/22132_l.jpg"
      recipe_tag: "Kids in the kitchen"
    -
      title: "Chicken nuggets"
      summary: "Why not let the kids create their own healthy version of chicken nuggets."
      img: "http://www.taste.com.au/images/recipes/tas/2012/01/27997_l.jpg"
      recipe_tag: "Kids in the kitchen"
    ...

In this example file:

* `title` and `summary` naturally map to Zotonic Page attributes.
* `img` can be mapped to `medium` (to import the image file)
* `recipe_tag` can be mapped to Keyword (if the Predicate connection allows this, and the Keyword with that title exists)


## Features

The import wizard offers the option to finetune the data before importing, to:

* rename fields
* select fields to exclude
* map the title field if it does not exist in the data
* map the fields to media type, to automatically download and import media files 
* map the fields to other data types (using a predicate)
* import a range

To be imported data can be previewed.

Before importing, you can choose Test Run (output is still rather basic).


## One category per file

The data file cannot have more than one data structure: all records should be structured the same. Split up the YAML file if you need to import pages in multiple categories.


## Importing multiple images

For the property to be mapped to medium, pass a comma-delimited string:

    %YAML 1.1
    ---
    # Recipes
    -
      title: "Cheesy Vegemite pull-apart"
      summary: "Put these cheesy vegemite scrolls into the school lunch-box as an exciting alternative to the sandwich."
      img: http://www.taste.com.au/images/recipes/sfi/2009/03/22132_l.jpg, http://www.taste.com.au/images/recipes/sfi/2010/03/24404_l.jpg
    ...

* At Data Type, choose Medium
* Check "Multiple import from comma-separated list"


## Importing categories

A YAML file to create new categories should have at least the attributes `title` and `name`. Example:

    %YAML 1.1
    ---
    -
      title: "Recipe"
      name: "recipe"
      is_published: "true"
    -
      title: "Recipe category"
      name: "recipe_category"
      is_published: "false"
    ...

1. Upload the data source.
2. At "Create Page type", choose "Category".
For attribute `is_published`, change Data Type from `text` (in the dialog, choose Status, then "is_published").


## Importing predicates

A YAML file to create new predicates should have 4 mandatory attributes for each predicate:

* `title`
* `name`: must be unique
* `from`: similar to Predicate edit screen - the category name (not title)
* `to`: similar to Predicate edit screen - the category name (not title)

All attributes have type Text.

Example:

    %YAML 1.1
    ---
    -
      title: "Has primary category"
      name: "has_primary_category"
      from: "navigation"
      to: "primary_category"
    ...


## Troubleshooting

* In case international characters are garbled: make sure the text is saved as UTF-8 (no BOM).
* In case the module does not import the file at all: make sure the list ends with a newline, and then 3 dots:

        ...


## Installation

See file INSTALL.


## TODO

* Better error reporting (also in Test Run mode).
* Support for translation strings.

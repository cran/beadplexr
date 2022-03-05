# beadplexr version 0.4.1

## Minor changes

+ Bumped R-dependency to 4.1
+ Fix problems related to `person()`
+ Fix minor problems due to general upgrade


# beadplexr version 0.4.0

## Breaking changes

 + All functionality to prepare flow-data has been removed due to license issues

## Major changes

 + The parameter `.data` is deprecated and `data.frames` are now passed by the parameter `df`. This is to avoid potential conflicts with the `dplyr` pronoun `.data`
 + All deprecated `tidyverse` functions are replaced with their stable counterparts

## Minor changes

+ Fix problems related to `tibble` version 3.0.0
+ Package now imports magrittr's pipe operator (%>%)
+ Package now imports rlang's walrus operator (:=)

# beadplexr version 0.3.0

## Minor changes

  + Bug fixes
  + Add BioLegend panel information
    + Mouse Anti-Virus Response Panel (13-plex)
    + Mouse Cytokine Panel 2 (13-plex)
    + Mouse Free Active/Total TGF-b1 Panel (Mouse/Rat) (1-plex)
    + Mouse HSC Panel (13-plex)
    + Mouse IgE Panel (1-plex)
    + Mouse Immunoglobulin Isotyping Panel (6-plex)
    + Mouse Inflammation Panel (13-plex)
    + Mouse Proinflammatory Chemokine Panel (13-plex)
    + Mouse T Helper Cytokine Panels (13-plex)

# beadplexr version 0.2.0

## Minor changes

  + Add BioLegend panel information
    + Human Adipokine Panel* (13-plex)
    + Human Anti-Virus Response Panel (13-plex)
    + Human CD8/NK Panel (13-plex)
    + Human Inflammation Panel (13-plex)
    + Human Metabolic Panel 1 (4-plex)
  + Add citation
  + Fix problem of missing column names after upgrade of `tibble` to 2.0

# beadplexr version 0.1.0

Initial release - clip the flag!

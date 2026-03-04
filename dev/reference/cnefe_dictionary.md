# Open the official CNEFE data dictionary

Opens the bundled Excel data dictionary in the system's default
spreadsheet viewer (e.g., Excel, LibreOffice).

## Usage

``` r
cnefe_dictionary(year = 2022)
```

## Arguments

- year:

  Integer. The CNEFE data year. Currently only 2022 is supported.

## Value

Invisibly, the path to the Excel file inside the installed package.

## Examples

``` r
if (FALSE) { # interactive()
cnefe_dictionary()
}
```

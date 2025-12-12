# Suppresses plot display in the IDE by opening a PDF graphics device

This function opens a PDF graphics device using
[`grDevices::pdf`](https://rdrr.io/r/grDevices/pdf.html) to suppress the
plot display in the IDE. The purpose of this function is to avoid
opening graphic devices directly in the IDE.

## Usage

``` r
dev_suppress(x)
```

## Arguments

- x:

  lazy binding which generates the plot(s)

## Value

No return value, called for side effects.

## Details

The function uses [`base::on.exit`](https://rdrr.io/r/base/on.exit.html)
to ensure that the PDF graphics device is closed (using
[`grDevices::dev.off`](https://rdrr.io/r/grDevices/dev.html)) when the
function exits, regardless of whether it exits normally or due to an
error. This is necessary to clean up the graphics device properly and
avoid any potential issues.

## Examples

``` r
dev_suppress(plot(1:10))
```

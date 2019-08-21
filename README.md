# dim-swap
[OSX release](https://github.com/quasi-coherent/dim-swap/releases)

## usage
Full list of options:

``` bash
dim-swap

Usage: dim-swap --file ARG -c ARG --col-min ARG --col-max ARG -r ARG
                  --row-min ARG --row-max ARG

Available options:
  -h,--help                Show this help text
  --file ARG               Image to sort
  -c ARG                   Integer in the range 0-100 representing the
                           percentage of columns to be swapped
  --col-min ARG            Integer in the range 0-100 representing where to
                           start random column swapping
  --col-max ARG            Integer in the range 0-100 representing where to end
                           random column swapping
  -r ARG                   Integer in the range 0-100 representing the
                           percentage of rows to be swapped
  --row-min ARG            Integer in the range 0-100 representing where to
                           start random row swapping
  --row-max ARG            Integer in the range 0-100 representing where to end
                           random row swapping
```

## examples
![original](repo_assets/image.png)

![0-0-100_95-50-100](repo_assets/glitched-0-0-100_95-50-100_image.png)

![100-0-100_100-0-100](repo_assets/glitched-100-0-100_100-0-100_image.png)

![100-50-100_100-30-100](repo_assets/glitched-100-50-100_100-30-100_image.png)

LiveHistogram
=============

```
  Usage: 
 
    LiveHistogram [options] min max 
 
Display a live histogram and count/mean/standard deviation statistics for
a stream of numbers from the standard input, where each number is on its 
own line. 
  min   : minimum value to be shown in the histogram 
  max   : maximum value to be shown in the histogram 

  -n x  --nbins=x    Use x histogram bins (default depends on terminal size).
  -e    --edges      Show the boundaries for each histogram bin.
  -r x  --refresh=x  Refresh screen at a rate of x Hz (default: 30 Hz).
        --help       Display this help message.
```

## Example

(Obviously, it's dynamic when actually run!)

```
> ./StdNormal | LiveHistogram -4 4
```

```
| <-- 0                                |                            25658 --> |


# 
### 
######### 
################### 
#################################### 
#######################################################  
########################################################################
###############################################################################
######################################################################### 
#######################################################  
####################################
####################
#########
###
#


Count    : 152983       Below    : 2            Above    : 4
Mean     : 9.234247608611908e-4     Std. Dev.: 0.999146316822345
Min      : -4.029982183932279       Max      : 4.443958676619477
```


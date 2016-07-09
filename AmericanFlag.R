# The MIT License (MIT)
# Copyright (c) 2016 Michael Davis
# Permission is hereby granted, free of charge, to any person obtaining a copy of this 
# software and associated documentation files (the "Software"), to deal in the Software 
# without restriction, including without limitation the rights to use, copy, modify, 
# merge, publish, distribute, sublicense, and/or sell copies of the Software, 
# and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
#   
# The above copyright notice and this permission notice shall be included in all 
# copies or substantial portions of the Software. 
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
# THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
# WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

flag <- function(){
  writeLines(c("This program calculates the dimensions of the American Flag using two questions. \n", "For more about the dimension definitions (e.g. A, B, C, D) go to Montney.com/flag/proportions.htm. \n", "We'll ask you if you want FEET or METERS in a second"))
  # dialogues to choose size then feet or meters (using readline)
  x <- readline(prompt="Enter your desired hoist (A: top of stripe facing skyward to bottom of bottom stripe): ")
  y <- readline(prompt="Enter 1 for dimensions in Feet OR 2 for dimensions in Metric: ")
     if (y == 1){
       chosen.dimension <- "Feet"
       alternate.dimension <- "Meters"
     }else{
       chosen.dimension <- "Meters"
       alternate.dimension <- "Feet"
     }
  # convert size input and measure to integer
  x <- as.double(x)
  y <- as.double(y)
    if (y == 1){
      x <- round(x, digits=2)
    } else {
      x <- round(x / 3.28, digits=2)
    }
  # x = size of flag (top of stripe nearest the sky to bottom of stripe nearest the ground)
  # next line is the size of the fly
  fly <- round(x * 1.9, digits=2)
  # hoist size of the union
  hoist.union <- round((7/13) * x, digits=2)
  # fly of the union
  fly.union <- round(x * .76, digits = 2)
  star.distance <- round((x * .063), digits = 2)
  #short red and then white stripes
  short.reds <- round((fly - fly.union), digits =2)
  short.whites <- round((fly - fly.union), digits =2)
  #the width of the stripes from the top of the stripe to the bottom
  stripes.width <- round(x / 13, digits=2)
  # the length of the long RED stripes then long WHITE stripes
  long.reds <- fly
  long.whites <- fly
  # total length of long and short RED then WHITE stripes
  total.reds <- round((4 * short.reds) + (3 * long.reds), digits=2)
  total.reds.metric <- round((total.reds / 3.28), digits = 2)
  total.whites <- round((3 * short.whites) + (3 * long.whites),digits=2)
  total.whites.metric <- round((total.whites / 3.28), digits = 2)
  # Now convert those lengths into the alternate measure 
  # e.g. if they chose ft.  for init. dimension, give it metric, too.
  cat("****EVERY DIMENSION below is in:", chosen.dimension, "**** \n")
  writeLines(c("Here's the overall dimensions:"))
  cat(c("    The Hoist of your flag (A: top of stripes to bottom of stripes) is: ", x, chosen.dimension, "\n"))
  cat(c("    The Fly of your flag (B: edge nearest pole to edge farthest away) is: ", fly, chosen.dimension, "\n"))
  writeLines("\n")
  writeLines(c("Now about the stripes:"))
  writeLines(c("The order of the stripes are (starting with one nearest the sky):"))
  writeLines(c("    R, W, R, W, R, W, R, W (starts the long stripes), R, W, R, W, R"))
  cat(c("    The width of the Stripes (L) are this wide: ", stripes.width, chosen.dimension, "\n"))
  cat(c("    The 4 Short Red Stripes (F) are this long: ", short.reds, chosen.dimension, "\n"))
  cat(c("    The 3 Short White Stripes (E) are this long: ", short.whites, chosen.dimension, "\n"))
  cat(c("    The 3 Long Red Stripes (H) are this long: ", long.reds, chosen.dimension, "\n"))
  cat(c("    The 3 Long White Stripes (H) are this long: ", long.whites, chosen.dimension, "\n"))
  writeLines(c("\n"))
  writeLines(c("Concerning the stars..."))
  cat(c("    The distance between the centers of the stars(side-to-side) is: ", star.distance, chosen.dimension, "\n"))
  cat(c("You will need this many total RED LED lights:", total.reds, "feet"," or ", total.reds.metric, "Meters", "\n" ))
  cat(c("You will need this many total WHITE LED lights:", total.whites, "Feet"," or ", total.whites.metric, "Meters", "\n"))
}

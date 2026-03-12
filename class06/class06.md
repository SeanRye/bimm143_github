# Class 06: R Functions
Sean Rye (PID: A17793986)

- [Background](#background)
- [Our first function](#our-first-function)
- [A second function](#a-second-function)
- [A Protein generating function](#a-protein-generating-function)

## Background

All functions in R have at least 3 things:

- A **name** that we use to call the function
- One or more input **arguments**
- The **body** the lines of R code that do the work

## Our first function

Let’s write a silly wee function called `add()` to add some numbers (the
input arguments)

``` r
add <- function(x, y) {
  x + y
}
```

Now we can use this function

``` r
add(100,1)
```

    [1] 101

``` r
add(c(100, 1, 100), 1)
```

    [1] 101   2 101

> Q. What if I give a multiple element vector to `x` and `y`?

``` r
add(x=c(100,1), y=c(100,1))
```

    [1] 200   2

> Q. What if I give three inputs to the function?

``` r
#add(x=c(100,1), y=1, z=1) 
```

> Q. What if I give only one input to the add function?

``` r
addnew <- function(x, y=1) {
  x + y
}
```

``` r
addnew(x=100)
```

    [1] 101

``` r
addnew(c(100,1),100)
```

    [1] 200 101

If we write our function with input arguments having no default value
then the user will be required to set them when they use the function.
We can give our input aguments “default” values by setting them equal to
some sensible value - e.g. y=1 int he `addnew()`

## A second function

Let’s try something more interesting: Make a sequence generating tool…

The `sample()` function can be a useful starting point here:

``` r
sample(1:10, size=4)
```

    [1] 8 5 4 3

> Q. Generate 9 random nmbers taken from the input vector x=1:10?

``` r
sample(1:10, size=9)
```

    [1]  9  5  3  6  8  4  7 10  1

> Q. Generate 12 random nmbers taken from the input vector x=1:10?

``` r
sample(1:10, size=12, replace=TRUE)
```

     [1]  3  3  1 10  8  2  2  6  5  8  2 10

> Q. Write code for the `sample()` function that generates nucleotide
> sequences of length 6?

``` r
sample(x=c("A","G","C","T"),size=6,replace=TRUE)
```

    [1] "A" "T" "T" "C" "C" "T"

> Q. Write a first function `generate_dna()` that returns a
> *user-specified length* DNA sequence:

``` r
generate_dna <- function(len=6) {
  sample(x=c("A","G","C","T"),size=len,replace=TRUE)
}
```

``` r
generate_dna()
```

    [1] "G" "G" "G" "G" "C" "G"

> **Key points** Every function in R looks fundamentally the same in
> terms of its structure. Basically 3 things: name, input, and body

    name <- function(input) {
    body
    } 

> Functions can have multiple inputs. These can be **required**
> arguments or **optional** arguments. With optional arguments having a
> set default value.

> Q. Modify and improve our `generate_dna()` function to return it’s
> generated sequence in a more standard form like “AGTAGTA” rather than
> the vector “A”, “C”, “G”, “A”

``` r
generate_dna <- function(len=6, fasta=TRUE) {
 ans <- sample(x=c("A","G","C","T"),size=len,replace=TRUE)
 
 if(fasta) {
   cat("Single-element vector output")
   ans <- paste(ans, collapse = "")
 } else{
   cat("Multi-element vector output")
 }
 
 return(ans)
}

generate_dna(fasta=T)
```

    Single-element vector output

    [1] "GTCGCC"

``` r
generate_dna(fasta=TRUE)
```

    Single-element vector output

    [1] "CATCGA"

The `paste()` function - it’s job is to join up or stick together
(a.k.a. paste) input strings together

``` r
paste(c("alice","Barry"), "loves R", sep="")
```

    [1] "aliceloves R" "Barryloves R"

Flow controls means here the R brain goes in your code

``` r
good_mood <- FALSE

if(good_mood) {
  cat("Great!")
} else{
  cat("Bummer!")
}
```

    Bummer!

## A Protein generating function

> Q. Write a function, called `generate_protein()` that generates a
> user-specified length of protein sequence.

``` r
generate_protein <- function(len=6, fasta=TRUE) {
 ans <-  sample(x=c("A","R","N","C","D","C","E","Q","G","H","I","L","K","G","M","F","P","S","T","W","Y","V"), size=len,replace=TRUE)

   if(fasta) {
   ans <- paste(ans, collapse = "")
 }
 return(ans)
 }

generate_protein(len=10, fasta=T)
```

    [1] "VYYAPTVWGE"

``` r
myseq <- generate_protein(40)
```

> Q. Use that function to generate random protein sequences between
> length 6 and 12

``` r
generate_protein(6)
```

    [1] "HEGCEC"

``` r
generate_protein(7)
```

    [1] "HDQLDEI"

``` r
generate_protein(8)
```

    [1] "KCWYHTCC"

``` r
generate_protein(9)
```

    [1] "CHLGPQPMR"

``` r
generate_protein(10)
```

    [1] "KINGGIGWYY"

``` r
generate_protein(11)
```

    [1] "WAPEISSGLGR"

``` r
generate_protein(12)
```

    [1] "SGCGDGAQHSWR"

``` r
for(i in 6:12) {
  # FASTA ID line ">id"
  cat(">", i,sep="","\n" )
  # Protein sequence line
  cat(generate_protein(i), "\n")
}
```

    >6
    SCCNKA 
    >7
    ESRGSIG 
    >8
    MFAFWTYW 
    >9
    EWHHEDKGD 
    >10
    LCQQYHNHWL 
    >11
    GNSLIHDFVVF 
    >12
    WFVWSTHWYPEM 

> Q. Are any of your sequences unique i.e not found anywhere in nature

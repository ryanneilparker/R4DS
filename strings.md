14 Strings
14.1 Introduction
library("tidyverse")
14.2 String basics
Exercise 14.2.1
In code that doesn’t use stringr, you’ll often see paste() and paste0(). What’s the difference between the two functions? What stringr function are they equivalent to? How do the functions differ in their handling of NA?

The function paste() separates strings by spaces by default, while paste0() does not separate strings with spaces by default.

paste("foo", "bar")
#> [1] "foo bar"
paste0("foo", "bar")
#> [1] "foobar"
Since str_c() does not separate strings with spaces by default it is closer in behavior to paste0().

str_c("foo", "bar")
#> [1] "foobar"
However, str_c() and the paste function handle NA differently. The function str_c() propagates NA, if any argument is a missing value, it returns a missing value. This is in line with how the numeric R functions, e.g. sum(), mean(), handle missing values. However, the paste functions, convert NA to the string "NA" and then treat it as any other character vector.

str_c("foo", NA)
#> [1] NA
paste("foo", NA)
#> [1] "foo NA"
paste0("foo", NA)
#> [1] "fooNA"
Exercise 14.2.2
In your own words, describe the difference between the sep and collapse arguments to str_c().

The sep argument is the string inserted between arguments to str_c(), while collapse is the string used to separate any elements of the character vector into a character vector of length one.

Exercise 14.2.3
Use str_length() and str_sub() to extract the middle character from a string. What will you do if the string has an even number of characters?

The following function extracts the middle character. If the string has an even number of characters the choice is arbitrary. We choose to select  
⌈
n
/
2
⌉
 , because that case works even if the string is only of length one. A more general method would allow the user to select either the floor or ceiling for the middle character of an even string.

x <- c("a", "abc", "abcd", "abcde", "abcdef")
L <- str_length(x)
m <- ceiling(L / 2)
str_sub(x, m, m)
#> [1] "a" "b" "b" "c" "c"
Exercise 14.2.4
What does str_wrap() do? When might you want to use it?

The function str_wrap() wraps text so that it fits within a certain width. This is useful for wrapping long strings of text to be typeset.

Exercise 14.2.5
What does str_trim() do? What’s the opposite of str_trim()?

The function str_trim() trims the whitespace from a string.

str_trim(" abc ")
#> [1] "abc"
str_trim(" abc ", side = "left")
#> [1] "abc "
str_trim(" abc ", side = "right")
#> [1] " abc"
The opposite of str_trim() is str_pad() which adds characters to each side.

str_pad("abc", 5, side = "both")
#> [1] " abc "
str_pad("abc", 4, side = "right")
#> [1] "abc "
str_pad("abc", 4, side = "left")
#> [1] " abc"
Exercise 14.2.6
Write a function that turns (e.g.) a vector c("a", "b", "c") into the string "a, b, and c". Think carefully about what it should do if given a vector of length 0, 1, or 2.

See the Chapter [Functions] for more details on writing R functions.

This function needs to handle four cases.

n == 0: an empty string, e.g. "".
n == 1: the original vector, e.g. "a".
n == 2: return the two elements separated by “and”, e.g. "a and b".
n > 2: return the first n - 1 elements separated by commas, and the last element separated by a comma and “and”, e.g. "a, b, and c".
str_commasep <- function(x, delim = ",") {
  n <- length(x)
  if (n == 0) {
    ""
  } else if (n == 1) {
    x
  } else if (n == 2) {
    # no comma before and when n == 2
    str_c(x[[1]], "and", x[[2]], sep = " ")
  } else {
    # commas after all n - 1 elements
    not_last <- str_c(x[seq_len(n - 1)], delim)
    # prepend "and" to the last element
    last <- str_c("and", x[[n]], sep = " ")
    # combine parts with spaces
    str_c(c(not_last, last), collapse = " ")
  }
}
str_commasep("")
#> [1] ""
str_commasep("a")
#> [1] "a"
str_commasep(c("a", "b"))
#> [1] "a and b"
str_commasep(c("a", "b", "c"))
#> [1] "a, b, and c"
str_commasep(c("a", "b", "c", "d"))
#> [1] "a, b, c, and d"
14.3 Matching patterns with regular expressions
14.3.1 Basic matches
Exercise 14.3.1.1
Explain why each of these strings don’t match a \: "\", "\\", "\\\".

"\": This will escape the next character in the R string.
"\\": This will resolve to \ in the regular expression, which will escape the next character in the regular expression.
"\\\": The first two backslashes will resolve to a literal backslash in the regular expression, the third will escape the next character. So in the regular expression, this will escape some escaped character.
Exercise 14.3.1.2
How would you match the sequence "'\ ?

str_view("\"'\\", "\"'\\\\", match = TRUE)
"'\
Exercise 14.3.1.3
What patterns will the regular expression \..\..\.. match? How would you represent it as a string?

It will match any patterns that are a dot followed by any character, repeated three times.

str_view(c(".a.b.c", ".a.b", "....."), c("\\..\\..\\.."), match = TRUE)
.a.b.c
14.3.2 Anchors
Exercise 14.3.2.1
How would you match the literal string "$^$"?

To check that the pattern works, I’ll include both the string "$^$", and an example where that pattern occurs in the middle of the string which should not be matched.

str_view(c("$^$", "ab$^$sfas"), "^\\$\\^\\$$", match = TRUE)
$^$
Exercise 14.3.2.2
Given the corpus of common words in stringr::words, create regular expressions that find all words that:

Start with “y”.
End with “x”
Are exactly three letters long. (Don’t cheat by using str_length()!)
Have seven letters or more.
Since this list is long, you might want to use the match argument to str_view() to show only the matching or non-matching words.

The answer to each part follows.

The words that start with “y” are:

str_view(stringr::words, "^y", match = TRUE)
year
yes
yesterday
yet
you
young
End with “x”

str_view(stringr::words, "x$", match = TRUE)
box
sex
six
tax
Are exactly three letters long are

str_view(stringr::words, "^...$", match = TRUE)
act
add
age
ago
air
all
and
any
arm
art
ask
bad
bag
bar
bed
bet
big
bit
box
boy
bus
but
buy
can
car
cat
cup
cut
dad
day
die
dog
dry
due
eat
egg
end
eye
far
few
fit
fly
for
fun
gas
get
god
guy
hit
hot
how
job
key
kid
lad
law
lay
leg
let
lie
lot
low
man
may
mrs
new
non
not
now
odd
off
old
one
out
own
pay
per
put
red
rid
run
say
see
set
sex
she
sir
sit
six
son
sun
tax
tea
ten
the
tie
too
top
try
two
use
war
way
wee
who
why
win
yes
yet
you
The words that have seven letters or more:

str_view(stringr::words, ".......", match = TRUE)
absolute
account
achieve
address
advertise
afternoon
against
already
alright
although
america
another
apparent
appoint
approach
appropriate
arrange
associate
authority
available
balance
because
believe
benefit
between
brilliant
britain
brother
business
certain
chairman
character
Christmas
colleague
collect
college
comment
committee
community
company
compare
complete
compute
concern
condition
consider
consult
contact
continue
contract
control
converse
correct
council
country
current
decision
definite
department
describe
develop
difference
difficult
discuss
district
document
economy
educate
electric
encourage
english
environment
especial
evening
evidence
example
exercise
expense
experience
explain
express
finance
fortune
forward
function
further
general
germany
goodbye
history
holiday
hospital
however
hundred
husband
identify
imagine
important
improve
include
increase
individual
industry
instead
interest
introduce
involve
kitchen
language
machine
meaning
measure
mention
million
minister
morning
necessary
obvious
occasion
operate
opportunity
organize
original
otherwise
paragraph
particular
pension
percent
perfect
perhaps
photograph
picture
politic
position
positive
possible
practise
prepare
present
pressure
presume
previous
private
probable
problem
proceed
process
produce
product
programme
project
propose
protect
provide
purpose
quality
quarter
question
realise
receive
recognize
recommend
relation
remember
represent
require
research
resource
respect
responsible
saturday
science
scotland
secretary
section
separate
serious
service
similar
situate
society
special
specific
standard
station
straight
strategy
structure
student
subject
succeed
suggest
support
suppose
surprise
telephone
television
terrible
therefore
thirteen
thousand
through
thursday
together
tomorrow
tonight
traffic
transport
trouble
tuesday
understand
university
various
village
wednesday
welcome
whether
without
yesterday
Since the pattern ....... is not anchored with either . or $ this will match any word with at last seven letters. The pattern, ^.......$, matches words with exactly seven characters.

14.3.3 Character classes and alternatives
Exercise 14.3.3.1
Create regular expressions to find all words that:

Start with a vowel.
That only contain consonants. (Hint: thinking about matching “not”-vowels.)
End with ed, but not with eed.
End with ing or ise.

The answer to each part follows.

Words starting with vowels

str_subset(stringr::words, "^[aeiou]")
#>   [1] "a"           "able"        "about"       "absolute"    "accept"     
#>   [6] "account"     "achieve"     "across"      "act"         "active"     
#>  [11] "actual"      "add"         "address"     "admit"       "advertise"  
#>  [16] "affect"      "afford"      "after"       "afternoon"   "again"      
#>  [21] "against"     "age"         "agent"       "ago"         "agree"      
#>  [26] "air"         "all"         "allow"       "almost"      "along"      
#>  [31] "already"     "alright"     "also"        "although"    "always"     
#>  [36] "america"     "amount"      "and"         "another"     "answer"     
#>  [41] "any"         "apart"       "apparent"    "appear"      "apply"      
#>  [46] "appoint"     "approach"    "appropriate" "area"        "argue"      
#>  [51] "arm"         "around"      "arrange"     "art"         "as"         
#>  [56] "ask"         "associate"   "assume"      "at"          "attend"     
#>  [61] "authority"   "available"   "aware"       "away"        "awful"      
#>  [66] "each"        "early"       "east"        "easy"        "eat"        
#>  [71] "economy"     "educate"     "effect"      "egg"         "eight"      
#>  [76] "either"      "elect"       "electric"    "eleven"      "else"       
#>  [81] "employ"      "encourage"   "end"         "engine"      "english"    
#>  [86] "enjoy"       "enough"      "enter"       "environment" "equal"      
#>  [91] "especial"    "europe"      "even"        "evening"     "ever"       
#>  [96] "every"       "evidence"    "exact"       "example"     "except"     
#> [101] "excuse"      "exercise"    "exist"       "expect"      "expense"    
#> [106] "experience"  "explain"     "express"     "extra"       "eye"        
#> [111] "idea"        "identify"    "if"          "imagine"     "important"  
#> [116] "improve"     "in"          "include"     "income"      "increase"   
#> [121] "indeed"      "individual"  "industry"    "inform"      "inside"     
#> [126] "instead"     "insure"      "interest"    "into"        "introduce"  
#> [131] "invest"      "involve"     "issue"       "it"          "item"       
#> [136] "obvious"     "occasion"    "odd"         "of"          "off"        
#> [141] "offer"       "office"      "often"       "okay"        "old"        
#> [146] "on"          "once"        "one"         "only"        "open"       
#> [151] "operate"     "opportunity" "oppose"      "or"          "order"      
#> [156] "organize"    "original"    "other"       "otherwise"   "ought"      
#> [161] "out"         "over"        "own"         "under"       "understand" 
#> [166] "union"       "unit"        "unite"       "university"  "unless"     
#> [171] "until"       "up"          "upon"        "use"         "usual"
Words that contain only consonants: Use the negate argument of str_subset.

str_subset(stringr::words, "[aeiou]", negate=TRUE)
#> [1] "by"  "dry" "fly" "mrs" "try" "why"
Alternatively, using str_view() the consonant-only words are:

str_view(stringr::words, "[aeiou]", match=FALSE)
by
dry
fly
mrs
try
why
Words that end with “-ed” but not ending in “-eed”.

str_subset(stringr::words, "[^e]ed$")
#> [1] "bed"     "hundred" "red"
The pattern above will not match the word "ed". If we wanted to include that, we could include it as a special case.

str_subset(c("ed", stringr::words), "(^|[^e])ed$")
#> [1] "ed"      "bed"     "hundred" "red"
Words ending in ing or ise:

str_subset(stringr::words, "i(ng|se)$")
#>  [1] "advertise" "bring"     "during"    "evening"   "exercise"  "king"     
#>  [7] "meaning"   "morning"   "otherwise" "practise"  "raise"     "realise"  
#> [13] "ring"      "rise"      "sing"      "surprise"  "thing"
Exercise 14.3.3.2
Empirically verify the rule “i” before e except after “c”.

length(str_subset(stringr::words, "(cei|[^c]ie)"))
#> [1] 14
length(str_subset(stringr::words, "(cie|[^c]ei)"))
#> [1] 3
Exercise 14.3.3.3
Is “q” always followed by a “u”?

In the stringr::words dataset, yes.

str_view(stringr::words, "q[^u]", match = TRUE)
by
dry
fly
mrs
try
why
In the English language— no. However, the examples are few, and mostly loanwords, such as “burqa” and “cinq”. Also, “qwerty”. That I had to add all of those examples to the list of words that spellchecking should ignore is indicative of their rarity.

Exercise 14.3.3.4
Write a regular expression that matches a word if it’s probably written in British English, not American English.

In the general case, this is hard, and could require a dictionary. But, there are a few heuristics to consider that would account for some common cases: British English tends to use the following:

“ou” instead of “o”
use of “ae” and “oe” instead of “a” and “o”
ends in ise instead of ize
ends in yse
The regex ou|ise$|ae|oe|yse$ would match these.

There are other spelling differences between American and British English but they are not patterns amenable to regular expressions. It would require a dictionary with differences in spellings for different words.

Exercise 14.3.3.5
Create a regular expression that will match telephone numbers as commonly written in your country.

<div class="alert alert-primary hints-alert> This answer can be improved and expanded.

The answer to this will vary by country.

For the United States, phone numbers have a format like 123-456-7890 or (123)456-7890). These regular expressions will parse the first form

x <- c("123-456-7890", "(123)456-7890", "(123) 456-7890", "1235-2351")
str_view(x, "\\d\\d\\d-\\d\\d\\d-\\d\\d\\d\\d")
123-456-7890
(123)456-7890
(123) 456-7890
1235-2351
str_view(x, "[0-9][0-9][0-9]-[0-9][0-9][0-9]-[0-9][0-9][0-9][0-9]")
123-456-7890
(123)456-7890
(123) 456-7890
1235-2351
The regular expressions will parse the second form:
str_view(x, "\\(\\d\\d\\d\\)\\s*\\d\\d\\d-\\d\\d\\d\\d")
123-456-7890
(123)456-7890
(123) 456-7890
1235-2351
str_view(x, "\\([0-9][0-9][0-9]\\)[ ]*[0-9][0-9][0-9]-[0-9][0-9][0-9][0-9]")
123-456-7890
(123)456-7890
(123) 456-7890
1235-2351
This regular expression can be simplified with the {m,n} regular expression modifier introduced in the next section,

str_view(x, "\\d{3}-\\d{3}-\\d{4}")
123-456-7890
(123)456-7890
(123) 456-7890
1235-2351
str_view(x, "\\(\\d{3}\\)\\s*\\d{3}-\\d{4}")
123-456-7890
(123)456-7890
(123) 456-7890
1235-2351
Note that this pattern doesn’t account for phone numbers that are invalid due to an invalid area code. Nor does this pattern account for special numbers like 911. It also doesn’t parse a leading country code or an extensions. See the Wikipedia page for the North American Numbering Plan for more information on the complexities of US phone numbers, and this Stack Overflow question for a discussion of using a regex for phone number validation. The R package dialr implements robust phone number parsing. Generally, for patterns like phone numbers or URLs it is better to use a dedicated package. It is easy to match the pattern for the most common cases and useful for learning regular expressions, but in real applications there often edge cases that are handled by dedicated packages.

14.3.4 Repetition
Exercise 14.3.4.1
Describe the equivalents of ?, +, * in {m,n} form.

Pattern	{m,n}	Meaning
?	{0,1}	Match at most 1
+	{1,}	Match 1 or more
*	{0,}	Match 0 or more
For example, let’s repeat the examples in the chapter, replacing ? with {0,1}, + with {1,}, and * with {*,}.

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")
1888 is the longest year in Roman numerals: MDCCCLXXXVIII
str_view(x, "CC{0,1}")
1888 is the longest year in Roman numerals: MDCCCLXXXVIII
str_view(x, "CC+")
1888 is the longest year in Roman numerals: MDCCCLXXXVIII
str_view(x, "CC{1,}")
1888 is the longest year in Roman numerals: MDCCCLXXXVIII
str_view_all(x, "C[LX]+")
1888 is the longest year in Roman numerals: MDCCCLXXXVIII
str_view_all(x, "C[LX]{1,}")
1888 is the longest year in Roman numerals: MDCCCLXXXVIII
The chapter does not contain an example of *. This pattern looks for a “C” optionally followed by any number of “L” or “X” characters.

str_view_all(x, "C[LX]*")
1888 is the longest year in Roman numerals: MDCCCLXXXVIII
str_view_all(x, "C[LX]{0,}")
1888 is the longest year in Roman numerals: MDCCCLXXXVIII
Exercise 14.3.4.2
Describe in words what these regular expressions match: (read carefully to see if I’m using a regular expression or a string that defines a regular expression.)

^.*$
"\\{.+\\}"
\d{4}-\d{2}-\d{2}
"\\\\{4}"
The answer to each part follows.

^.*$ will match any string. For example: ^.*$: c("dog", "$1.23", "lorem ipsum").

"\\{.+\\}" will match any string with curly braces surrounding at least one character. For example: "\\{.+\\}": c("{a}", "{abc}").

\d{4}-\d{2}-\d{2} will match four digits followed by a hyphen, followed by two digits followed by a hyphen, followed by another two digits. This is a regular expression that can match dates formatted like “YYYY-MM-DD” (“%Y-%m-%d”). For example: \d{4}-\d{2}-\d{2}: 2018-01-11

"\\\\{4}" is \\{4}, which will match four backslashes. For example: "\\\\{4}": "\\\\\\\\".

Exercise 14.3.4.3
Create regular expressions to find all words that:

Start with three consonants.
Have three or more vowels in a row.
Have two or more vowel-consonant pairs in a row.
The answer to each part follows.

This regex finds all words starting with three consonants.

str_view(words, "^[^aeiou]{3}", match = TRUE)
Christ
Christmas
dry
fly
mrs
scheme
school
straight
strategy
street
strike
strong
structure
system
three
through
throw
try
type
why
This regex finds three or more vowels in a row:

str_view(words, "[aeiou]{3,}", match = TRUE)
beauty
obvious
previous
quiet
serious
various
This regex finds two or more vowel-consonant pairs in a row.

str_view(words, "([aeiou][^aeiou]){2,}", match = TRUE)
absolute
agent
along
america
another
apart
apparent
authority
available
aware
away
balance
basis
become
before
begin
behind
benefit
business
character
closes
community
consider
cover
debate
decide
decision
definite
department
depend
design
develop
difference
difficult
direct
divide
document
during
economy
educate
elect
electric
eleven
encourage
environment
europe
even
evening
ever
every
evidence
exact
example
exercise
exist
family
figure
final
finance
finish
friday
future
general
govern
holiday
honest
hospital
however
identify
imagine
individual
interest
introduce
item
jesus
level
likely
limit
local
major
manage
meaning
measure
minister
minus
minute
moment
money
music
nature
necessary
never
notice
okay
open
operate
opportunity
organize
original
over
paper
paragraph
parent
particular
photograph
police
policy
politic
position
positive
power
prepare
present
presume
private
probable
process
produce
product
project
proper
propose
protect
provide
quality
realise
reason
recent
recognize
recommend
record
reduce
refer
regard
relation
remember
report
represent
result
return
saturday
second
secretary
secure
separate
seven
similar
specific
strategy
student
stupid
telephone
television
therefore
thousand
today
together
tomorrow
tonight
total
toward
travel
unit
unite
university
upon
visit
water
woman
Exercise 14.3.4.4
Solve the beginner regexp crosswords at https://regexcrossword.com/challenges/

Exercise left to reader. That site validates its solutions, so they aren’t repeated here.

14.3.5 Grouping and backreferences
Exercise 14.3.5.1
Describe, in words, what these expressions will match:

(.)\1\1 :
"(.)(.)\\2\\1":
(..)\1:
"(.).\\1.\\1":
"(.)(.)(.).*\\3\\2\\1"
The answer to each part follows.

(.)\1\1: The same character appearing three times in a row. E.g. "aaa"
"(.)(.)\\2\\1": A pair of characters followed by the same pair of characters in reversed order. E.g. "abba".
(..)\1: Any two characters repeated. E.g. "a1a1".
"(.).\\1.\\1": A character followed by any character, the original character, any other character, the original character again. E.g. "abaca", "b8b.b".
"(.)(.)(.).*\\3\\2\\1" Three characters followed by zero or more characters of any kind followed by the same three characters but in reverse order. E.g. "abcsgasgddsadgsdgcba" or "abccba" or "abc1cba".
Exercise 14.3.5.2
Construct regular expressions to match words that:

Start and end with the same character.
Contain a repeated pair of letters (e.g. church'' containsch’’ repeated twice.)
Contain one letter repeated in at least three places (e.g. eleven'' contains threee’’s.)
The answer to each part follows.

This regular expression matches words that start and end with the same character.

str_subset(words, "^(.)((.*\\1$)|\\1?$)")
#>  [1] "a"          "america"    "area"       "dad"        "dead"      
#>  [6] "depend"     "educate"    "else"       "encourage"  "engine"    
#> [11] "europe"     "evidence"   "example"    "excuse"     "exercise"  
#> [16] "expense"    "experience" "eye"        "health"     "high"      
#> [21] "knock"      "level"      "local"      "nation"     "non"       
#> [26] "rather"     "refer"      "remember"   "serious"    "stairs"    
#> [31] "test"       "tonight"    "transport"  "treat"      "trust"     
#> [36] "window"     "yesterday"
This regular expression will match any pair of repeated letters, where letters is defined to be the ASCII letters A-Z. First, check that it works with the example in the problem.

str_subset("church", "([A-Za-z][A-Za-z]).*\\1")
#> [1] "church"
Now, find all matching words in words.

str_subset(words, "([A-Za-z][A-Za-z]).*\\1")
#>  [1] "appropriate" "church"      "condition"   "decide"      "environment"
#>  [6] "london"      "paragraph"   "particular"  "photograph"  "prepare"    
#> [11] "pressure"    "remember"    "represent"   "require"     "sense"      
#> [16] "therefore"   "understand"  "whether"
The \\1 pattern is called a backreference. It matches whatever the first group matched. This allows the pattern to match a repeating pair of letters without having to specify exactly what pair letters is being repeated.

Note that these patterns are case sensitive. Use the case insensitive flag if you want to check for repeated pairs of letters with different capitalization.

This regex matches words that contain one letter repeated in at least three places. First, check that it works with th example given in the question.

str_subset("eleven", "([a-z]).*\\1.*\\1")
#> [1] "eleven"
Now, retrieve the matching words in words.

str_subset(words, "([a-z]).*\\1.*\\1")
#>  [1] "appropriate" "available"   "believe"     "between"     "business"   
#>  [6] "degree"      "difference"  "discuss"     "eleven"      "environment"
#> [11] "evidence"    "exercise"    "expense"     "experience"  "individual" 
#> [16] "paragraph"   "receive"     "remember"    "represent"   "telephone"  
#> [21] "therefore"   "tomorrow"
14.4 Tools
14.4.1 Detect matches
Exercise 14.4.1.1
For each of the following challenges, try solving it by using both a single regular expression, and a combination of multiple str_detect() calls.

Find all words that start or end with x.
Find all words that start with a vowel and end with a consonant.
Are there any words that contain at least one of each different vowel?
The answer to each part follows.

Words that start or end with x?

# one regex
words[str_detect(words, "^x|x$")]
#> [1] "box" "sex" "six" "tax"
# split regex into parts
start_with_x <- str_detect(words, "^x")
end_with_x <- str_detect(words, "x$")
words[start_with_x | end_with_x]
#> [1] "box" "sex" "six" "tax"
Words starting with vowel and ending with consonant.

str_subset(words, "^[aeiou].*[^aeiou]$") %>% head()
#> [1] "about"   "accept"  "account" "across"  "act"     "actual"
start_with_vowel <- str_detect(words, "^[aeiou]")
end_with_consonant <- str_detect(words, "[^aeiou]$")
words[start_with_vowel & end_with_consonant] %>% head()
#> [1] "about"   "accept"  "account" "across"  "act"     "actual"
There is not a simple regular expression to match words that that contain at least one of each vowel. The regular expression would need to consider all possible orders in which the vowels could occur.

pattern <-
  cross(rerun(5, c("a", "e", "i", "o", "u")),
    .filter = function(...) {
      x <- as.character(unlist(list(...)))
      length(x) != length(unique(x))
    }
  ) %>%
  map_chr(~str_c(unlist(.x), collapse = ".*")) %>%
  str_c(collapse = "|")
To check that this pattern works, test it on a pattern that should match

str_subset("aseiouds", pattern)
#> [1] "aseiouds"
Using multiple str_detect() calls, one pattern for each vowel, produces a much simpler and readable answer.

str_subset(words, pattern)
#> character(0)

words[str_detect(words, "a") &
  str_detect(words, "e") &
  str_detect(words, "i") &
  str_detect(words, "o") &
  str_detect(words, "u")]
#> character(0)
There appear to be none.

Exercise 14.4.1.2
What word has the higher number of vowels? What word has the highest proportion of vowels? (Hint: what is the denominator?)

The word with the highest number of vowels is

vowels <- str_count(words, "[aeiou]")
words[which(vowels == max(vowels))]
#> [1] "appropriate" "associate"   "available"   "colleague"   "encourage"  
#> [6] "experience"  "individual"  "television"
The word with the highest proportion of vowels is

prop_vowels <- str_count(words, "[aeiou]") / str_length(words)
words[which(prop_vowels == max(prop_vowels))]
#> [1] "a"
14.4.2 Extract matches
Exercise 14.4.2.1
In the previous example, you might have noticed that the regular expression matched “flickered”, which is not a color. Modify the regex to fix the problem.

This was the original color match pattern:

colours <- c("red", "orange", "yellow", "green", "blue", "purple")
colour_match <- str_c(colours, collapse = "|")
It matches “flickered” because it matches “red”. The problem is that the previous pattern will match any word with the name of a color inside it. We want to only match colors in which the entire word is the name of the color. We can do this by adding a \b (to indicate a word boundary) before and after the pattern:

colour_match2 <- str_c("\\b(", str_c(colours, collapse = "|"), ")\\b")
colour_match2
#> [1] "\\b(red|orange|yellow|green|blue|purple)\\b"
more2 <- sentences[str_count(sentences, colour_match) > 1]
str_view_all(more2, colour_match2, match = TRUE)
It is hard to erase blue or red ink.
The green light in the brown box flickered.
The sky in the west is tinged with orange red.
Exercise 14.4.2.2
From the Harvard sentences data, extract:

The first word from each sentence.
All words ending in ing.
All plurals.
The answer to each part follows.

Finding the first word in each sentence requires defining what a pattern constitutes a word. For the purposes of this question, I’ll consider a word any contiguous set of letters. Since str_extract() will extract the first match, if it is provided a regular expression for words, it will return the first word.

str_extract(sentences, "[A-ZAa-z]+") %>% head()
#> [1] "The"   "Glue"  "It"    "These" "Rice"  "The"
However, the third sentence begins with “It’s”. To catch this, I’ll change the regular expression to require the string to begin with a letter, but allow for a subsequent apostrophe.

str_extract(sentences, "[A-Za-z][A-Za-z']*") %>% head()
#> [1] "The"   "Glue"  "It's"  "These" "Rice"  "The"
This pattern finds all words ending in ing.

pattern <- "\\b[A-Za-z]+ing\\b"
sentences_with_ing <- str_detect(sentences, pattern)
unique(unlist(str_extract_all(sentences[sentences_with_ing], pattern))) %>%
  head()
#> [1] "spring"  "evening" "morning" "winding" "living"  "king"
Finding all plurals cannot be correctly accomplished with regular expressions alone. Finding plural words would at least require morphological information about words in the language. See WordNet for a resource that would do that. However, identifying words that end in an “s” and with more than three characters, in order to remove “as”, “is”, “gas”, etc., is a reasonable heuristic.

unique(unlist(str_extract_all(sentences, "\\b[A-Za-z]{3,}s\\b"))) %>%
  head()
#> [1] "planks" "days"   "bowls"  "lemons" "makes"  "hogs"
14.4.3 Grouped matches
Exercise 14.4.3.1
Find all words that come after a “number” like “one”, “two”, “three” etc. Pull out both the number and the word.

numword <- "\\b(one|two|three|four|five|six|seven|eight|nine|ten) +(\\w+)"
sentences[str_detect(sentences, numword)] %>%
  str_extract(numword)
#>  [1] "seven books"   "two met"       "two factors"   "three lists"  
#>  [5] "seven is"      "two when"      "ten inches"    "one war"      
#>  [9] "one button"    "six minutes"   "ten years"     "two shares"   
#> [13] "two distinct"  "five cents"    "two pins"      "five robins"  
#> [17] "four kinds"    "three story"   "three inches"  "six comes"    
#> [21] "three batches" "two leaves"
Exercise 14.4.3.2
Find all contractions. Separate out the pieces before and after the apostrophe.

This is done in two steps. First, identify the contractions. Second, split the string on the contraction.

contraction <- "([A-Za-z]+)'([A-Za-z]+)"
sentences[str_detect(sentences, contraction)] %>%
  str_extract(contraction) %>%
  str_split("'")
#> [[1]]
#> [1] "It" "s" 
#> 
#> [[2]]
#> [1] "man" "s"  
#> 
#> [[3]]
#> [1] "don" "t"  
#> 
#> [[4]]
#> [1] "store" "s"    
#> 
#> [[5]]
#> [1] "workmen" "s"      
#> 
#> [[6]]
#> [1] "Let" "s"  
#> 
#> [[7]]
#> [1] "sun" "s"  
#> 
#> [[8]]
#> [1] "child" "s"    
#> 
#> [[9]]
#> [1] "king" "s"   
#> 
#> [[10]]
#> [1] "It" "s" 
#> 
#> [[11]]
#> [1] "don" "t"  
#> 
#> [[12]]
#> [1] "queen" "s"    
#> 
#> [[13]]
#> [1] "don" "t"  
#> 
#> [[14]]
#> [1] "pirate" "s"     
#> 
#> [[15]]
#> [1] "neighbor" "s"
14.4.4 Replacing matches
Exercise 14.4.4.1
Replace all forward slashes in a string with backslashes.

str_replace_all("past/present/future", "/", "\\\\")
#> [1] "past\\present\\future"
Exercise 14.4.4.2
Implement a simple version of str_to_lower() using replace_all().

replacements <- c("A" = "a", "B" = "b", "C" = "c", "D" = "d", "E" = "e",
                  "F" = "f", "G" = "g", "H" = "h", "I" = "i", "J" = "j", 
                  "K" = "k", "L" = "l", "M" = "m", "N" = "n", "O" = "o", 
                  "P" = "p", "Q" = "q", "R" = "r", "S" = "s", "T" = "t", 
                  "U" = "u", "V" = "v", "W" = "w", "X" = "x", "Y" = "y", 
                  "Z" = "z")
lower_words <- str_replace_all(words, pattern = replacements)
head(lower_words)
#> [1] "a"        "able"     "about"    "absolute" "accept"   "account"
Exercise 14.4.4.3
Switch the first and last letters in words. Which of those strings are still words?

First, make a vector of all the words with first and last letters swapped,

swapped <- str_replace_all(words, "^([A-Za-z])(.*)([A-Za-z])$", "\\3\\2\\1")
Next, find what of “swapped” is also in the original list using the function intersect(),

intersect(swapped, words)
#>  [1] "a"          "america"    "area"       "dad"        "dead"      
#>  [6] "lead"       "read"       "depend"     "god"        "educate"   
#> [11] "else"       "encourage"  "engine"     "europe"     "evidence"  
#> [16] "example"    "excuse"     "exercise"   "expense"    "experience"
#> [21] "eye"        "dog"        "health"     "high"       "knock"     
#> [26] "deal"       "level"      "local"      "nation"     "on"        
#> [31] "non"        "no"         "rather"     "dear"       "refer"     
#> [36] "remember"   "serious"    "stairs"     "test"       "tonight"   
#> [41] "transport"  "treat"      "trust"      "window"     "yesterday"
Alternatively, the regex can be written using the POSIX character class for letter ([[:alpha:]]):

swapped2 <- str_replace_all(words, "^([[:alpha:]])(.*)([[:alpha:]])$", "\\3\\2\\1")
intersect(swapped2, words)
#>  [1] "a"          "america"    "area"       "dad"        "dead"      
#>  [6] "lead"       "read"       "depend"     "god"        "educate"   
#> [11] "else"       "encourage"  "engine"     "europe"     "evidence"  
#> [16] "example"    "excuse"     "exercise"   "expense"    "experience"
#> [21] "eye"        "dog"        "health"     "high"       "knock"     
#> [26] "deal"       "level"      "local"      "nation"     "on"        
#> [31] "non"        "no"         "rather"     "dear"       "refer"     
#> [36] "remember"   "serious"    "stairs"     "test"       "tonight"   
#> [41] "transport"  "treat"      "trust"      "window"     "yesterday"
14.4.5 Splitting
Exercise 14.4.5.1
Split up a string like "apples, pears, and bananas" into individual components.

x <- c("apples, pears, and bananas")
str_split(x, ", +(and +)?")[[1]]
#> [1] "apples"  "pears"   "bananas"
Exercise 14.4.5.2
Why is it better to split up by boundary("word") than " "?

Splitting by boundary("word") is a more sophisticated method to split a string into words. It recognizes non-space punctuation that splits words, and also removes punctuation while retaining internal non-letter characters that are parts of the word, e.g., “can’t” See the ICU website for a description of the set of rules that are used to determine word boundaries.

Consider this sentence from the official Unicode Report on word boundaries,

sentence <- "The quick (“brown”) fox can’t jump 32.3 feet, right?"
Splitting the string on spaces considers will group the punctuation with the words,

str_split(sentence, " ")
#> [[1]]
#> [1] "The"       "quick"     "(“brown”)" "fox"       "can’t"     "jump"     
#> [7] "32.3"      "feet,"     "right?"
However, splitting the string using boundary("word") correctly removes punctuation, while not separating “32.2” and “can’t”,

str_split(sentence, boundary("word"))
#> [[1]]
#> [1] "The"   "quick" "brown" "fox"   "can’t" "jump"  "32.3"  "feet"  "right"
Exercise 14.4.5.3
What does splitting with an empty string ("") do? Experiment, and then read the documentation.

str_split("ab. cd|agt", "")[[1]]
#>  [1] "a" "b" "." " " "c" "d" "|" "a" "g" "t"
It splits the string into individual characters.

14.4.6 Find matches
No exercises
14.5 Other types of pattern
Exercise 14.5.1
How would you find all strings containing \ with regex() vs. with fixed()?

str_subset(c("a\\b", "ab"), "\\\\")
#> [1] "a\\b"
str_subset(c("a\\b", "ab"), fixed("\\"))
#> [1] "a\\b"
Exercise 14.5.2
What are the five most common words in sentences?

Using str_extract_all() with the argument boundary("word") will extract all words. The rest of the code uses dplyr functions to count words and find the most common words.

tibble(word = unlist(str_extract_all(sentences, boundary("word")))) %>%
  mutate(word = str_to_lower(word)) %>%
  count(word, sort = TRUE) %>%
  head(5)
#> # A tibble: 5 x 2
#>   word      n
#>   <chr> <int>
#> 1 the     751
#> 2 a       202
#> 3 of      132
#> 4 to      123
#> 5 and     118
14.6 Other uses of regular expressions
No exercises
14.7 stringi
library("stringi")
Exercise 14.7.1
Find the stringi functions that:

Count the number of words.
Find duplicated strings.
Generate random text.
The answer to each part follows.

To count the number of words use stringi::stri_count_words(). This code counts the words in the first five sentences of sentences.

stri_count_words(head(sentences))
#> [1] 8 8 9 9 7 7
The stringi::stri_duplicated() function finds duplicate strings.

stri_duplicated(c("the", "brown", "cow", "jumped", "over",
                           "the", "lazy", "fox"))
#> [1] FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE
The stringi package contains several functions beginning with stri_rand_* that generate random text. The function stringi::stri_rand_strings() generates random strings. The following code generates four random strings each of length five.

stri_rand_strings(4, 5)
#> [1] "5pb90" "SUHjl" "sA2JO" "CP3Oy"
The function stringi::stri_rand_shuffle() randomly shuffles the characters in the text.

stri_rand_shuffle("The brown fox jumped over the lazy cow.")
#> [1] "ot f.lween p   jzwoom xyucobhv daheerrT"
The function stringi::stri_rand_lipsum() generates lorem ipsum text. Lorem ipsum text is nonsense text often used as placeholder text in publishing. The following code generates one paragraph of placeholder text.

stri_rand_lipsum(1)
#> [1] "Lorem ipsum dolor sit amet, hac non metus cras nam vitae tempus proin, sed. Diam gravida viverra eros mauris, magna lacinia dui nullam. Arcu proin aenean fringilla sed sollicitudin hac neque, egestas condimentum massa, elementum vivamus. Odio eget litora molestie eget eros pulvinar ac. Vel nec nullam vivamus, sociosqu lectus varius eleifend. Vitae in. Conubia ut hac maximus amet, conubia sed. Volutpat vitae class cursus, elit mauris porta. Mauris lacus donec odio eget quam inceptos, ridiculus cursus, ad massa. Rhoncus hac aenean at id consectetur molestie vitae! Sed, primis mi dictum lacinia eros. Ligula, feugiat consequat ut vivamus ut morbi et. Dolor, eget eleifend nec magnis aliquam egestas. Sollicitudin venenatis et aptent rhoncus nisl platea ligula cum."
Exercise 14.7.2
How do you control the language that stri_sort() uses for sorting?

You can set a locale to use when sorting with either stri_sort(..., opts_collator=stri_opts_collator(locale = ...)) or stri_sort(..., locale = ...). In this example from the stri_sort() documentation, the sorted order of the character vector depends on the locale.

string1 <- c("hladny", "chladny")
stri_sort(string1, locale = "pl_PL")
#> [1] "chladny" "hladny"
stri_sort(string1, locale = "sk_SK")
#> [1] "hladny"  "chladny"
The output of stri_opts_collator() can also be used for the locale argument of str_sort.

stri_sort(string1, opts_collator = stri_opts_collator(locale = "pl_PL"))
#> [1] "chladny" "hladny"
stri_sort(string1, opts_collator = stri_opts_collator(locale = "sk_SK"))
#> [1] "hladny"  "chladny"
The stri_opts_collator() provides finer grained control over how strings are sorted. In addition to setting the locale, it has options to customize how cases, unicode, accents, and numeric values are handled when comparing strings.

string2 <- c("number100", "number2")
stri_sort(string2)
#> [1] "number100" "number2"
stri_sort(string2, opts_collator = stri_opts_collator(numeric = TRUE))
#> [1] "number2"   "number100"
 

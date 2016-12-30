
{--
list of prime numbers
to get a list of primes, we will use a "list comprehension".
-firstly, we define a function for factors of number "n"
-as we know A prime number is a whole number greater than 1, whose only two whole-number factors are 1 and itself.
-here we are to get the list of all prime numbers lower than given number "k", we use a list comprehension.
--}
{--Factors of a given number n --}

factors:: Int->[Int]
factors k=[x | x <-[1..k], k`mod`x == 0 ]

{--so it's easy now to decide a number is prime or not, so we have to test if the list of all factors has just two members 1, and the number itself--}

prime n = factors n ==[1,n]

{--after that a list of prime numbers lower than a given k, is a list--}

primes n =[x | x<-[2..n], prime x]

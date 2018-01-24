from functools import lru_cache

# Goal: compute the values of the Fibonacci sequence at 100, 500, 1000, 10**5, and 10**6
# For each of the definitions, try how big a value you can compute. Why does it
# fail there?

# First define fib as a recursive function.
def fib(n):
    if n == 0 or n == 1 :
        return n
    else:
        return fib(n-1)+fib(n-2)


# Define fib as a recursive function, but using the lru_cache decorater to
# memoize results.
@lru_cache()
def fib_cache(n):
    if n <= 1:
        return n
    else:
        return fib_cache(n-1)+fib_cache(n-2)

# Draw the call tree for n = 5 and identify which subproblems are repeated.

#   MAKE A DRAWING


# Define fib recursively and manually memoize results.

slovar = {}
slovar[0] = 0
slovar[1] = 1
def fib_memo_rec(n):
    if (n-1) in slovar:
        x = slovar[n-1]
    else:
        x = fib_memo_rec(n-1)
        slovar[n-1] = x
    if (n-2) in slovar:
        y = slovar[n-2]
    else:
        y = fib_memo_rec(n-2)
        slovar[n-2] = y
    return x + y






# Make a new drawing where you merge the repeated nodes in the tree. Which
# subproblems does each call depend on directly?

#   I DON'T SEE YOU DRAWING


# Define fib as a dynamic program that fills up the table of results from the bottom.
s_iter = {}
s_iter[0] = 0
s_iter[1] = 1
def fib_memo_iter(n):
    for i in range (2,n+1):
        val = s_iter[i-1] + s_iter[i-2]
        s_iter[i] = val
    return s_iter[n]

# Define fib as a dynamic program that only keeps those intermediate results
# around that are needed to compute the next step.
#def fib_iter(n):
s_iter_m = {}
s_iter_m[0] = 0
s_iter_m[1] = 1
def fib_iter(n):
    x1 = 0
    x2= 1
    for i in range (2,n+1):
        x3= x1 + x2
        x1 = x2
        x2 = x3
    return x2

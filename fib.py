from functools import lru_cache

# Goal: compute the values of the Fibonacci sequence at 100, 500, 1000, 10**5, and 10**6
# For each of the definitions, try how big a value you can compute. Why does it
# fail there?

# First define fib as a recursive function.
def fib(n):
    if n in [0,1]:
        return n
    else:
        return fib(n-1) + fib(n-2)

# Define fib as a recursive function, but using the lru_cache decorater to
# memoize results.
@lru_cache(maxsize = None)
def fib_cache(n):
    print(n)
    if n in [0,1]:
        return n
    else:
        return fib_cache(n-1) + fib_cache(n-2)
    

# Draw the call tree for n = 5 and identify which subproblems are repeated.

#   MAKE A DRAWING


# Define fib recursively and manually memoize results.
#def fib_memo_rec(n):

# Make a new drawing where you merge the repeated nodes in the tree. Which
# subproblems does each call depend on directly?

#   I DON'T SEE YOU DRAWING


# Define fib as a dynamic program that fills up the table of results from the bottom.
#def fib_memo_iter(n):

# Define fib as a dynamic program that only keeps those intermediate results
# around that are needed to compute the next step.
#def fib_iter(n):

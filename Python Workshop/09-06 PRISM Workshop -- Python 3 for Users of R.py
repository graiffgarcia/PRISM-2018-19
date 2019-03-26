def prime_finder(n):
    integers = range(2, n+1)
    divides = {}
    zero_remainders = lambda x, y: [x % i == 0 for i in y]
    for i in range(len(integers)-1):
        divides[i+2] = zero_remainders(integers[i], [x for x in integers if x < integers[i]**1/2])
    return {k for k, v in divides.items() if sum(v) == 0}

num = raw_input("Highest number?")

prime_finder(num)
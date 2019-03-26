# def prime_finder(n):
#     integers = range(2, n+1)
#     divides = dict.fromkeys(list(integers))
#     primes = [2]
#     zero_remainders = lambda x, y: [x % i == 0 for i in y]
#     for i in list(integers):
#         divides[i] = zero_remainders(i, primes)
#         if sum(divides[i]) == 0:
#             primes.append(i)
#     print(primes)

# num = int(input("Please enter the highest number you want to test: "))

# prime_finder(num)

def prime_finder(n):
    integers = range(2, n+1)
    divides = dict.fromkeys(list(integers))
    primes = [2]
    zero_remainders = lambda x, y: [x % i == 0 for i in y]
    for i in list(integers):
        if(sum(zero_remainders(i, primes)) == 0):
            print(f'{i} is prime!')
            primes.append(i)

num = int(input("Please enter the highest number you want to test: "))

prime_finder(num)
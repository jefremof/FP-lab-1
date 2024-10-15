
def is_prime(n):
    if n <= 1:
        return False

    for i in range(2, int(n**0.5) + 1):
        if n % i == 0:
            return False

    return True

def count_primes(a, b):
    n = 0
    while True:
        value = n * n + a * n + b
        if (not is_prime(value)):
            return n
        n += 1

def solution(limit_a, limit_b):
    max_count = 0
    max_product = 0
    for b in range(2, limit_b + 1):
        if is_prime(b):
            for a in range(-limit_a, limit_a + 1):
                if a % 2 == b % 2:
                    count = count_primes(a, b)
                    if (count > max_count):
                        max_count = count
                        max_product = a * b
    return max_product

result = solution(999, 999)
print("Solution 6 (python):", result)

assert (result == -59231)

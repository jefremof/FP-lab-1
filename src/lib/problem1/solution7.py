def is_palindrome(n):
    return str(n) == str(n)[::-1]

def largest_palindrome(lower, upper):
    assert (upper > lower and lower > 0)
    champ = 0
    for i in range(upper, lower, -1):
        for j in range(i, lower, -1):
            product = i * j
            if is_palindrome(product):
                champ = max(champ, product)
    assert (champ > 0)
    return champ

result = largest_palindrome(100, 999)
print("Solution 6 (python):", result)

assert (result == 906609)

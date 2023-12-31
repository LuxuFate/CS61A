"""Optional questions for Lab 1"""

# While Loops

def falling(n, k):
    """Compute the falling factorial of n to depth k.

    >>> falling(6, 3)  # 6 * 5 * 4
    120
    >>> falling(4, 3)  # 4 * 3 * 2
    24
    >>> falling(4, 1)  # 4
    4
    >>> falling(4, 0)
    1
    """
    f=1
    if(k>0):
        f=n
    while(k>1):
        f=f*(n-1)
        n-=1
        k-=1

    return f
    "*** YOUR CODE HERE ***"

def double_eights(n):
    """Return true if n has two eights in a row.
    >>> double_eights(8)
    False
    >>> double_eights(88)
    True
    >>> double_eights(2882)
    True
    >>> double_eights(880088)
    True
    >>> double_eights(12345)
    False
    >>> double_eights(80808080)
    False
    """
    while(n>0):
        if(n%10 == 8):
            n=n//10
            if(n%10 == 8):
                return True
        else:
            n = n//10

    return False


    "*** YOUR CODE HERE ***"

i: int|bool|str
i = 2

def f(i: int, j: int)-> int:
    return i + j

a:int
a = f(1, 2)
a:int = f(1, 2.)
# a = f(1, 2.0)  # This should raise a type error since 2.0 is not an int
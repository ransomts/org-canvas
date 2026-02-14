"""DS 101 - Lab 01: Python Basics (Starter Code)

Complete each TODO below. Run the script to verify your answers.
"""


def greet(name):
    """Return a greeting string for the given name."""
    # TODO: Return "Hello, <name>!" where <name> is the argument
    pass


def is_even(n):
    """Return True if n is even, False otherwise."""
    # TODO: Implement this function
    pass


def sum_list(numbers):
    """Return the sum of all numbers in the list."""
    # TODO: Use a loop or built-in function to compute the sum
    pass


def filter_positives(numbers):
    """Return a new list containing only the positive numbers."""
    # TODO: Use a list comprehension to filter positive values
    pass


def count_vowels(text):
    """Return the number of vowels (a, e, i, o, u) in the text."""
    # TODO: Count vowels (case-insensitive)
    pass


# --- Verification ---
if __name__ == "__main__":
    assert greet("Alice") == "Hello, Alice!"
    assert is_even(4) is True
    assert is_even(7) is False
    assert sum_list([1, 2, 3, 4]) == 10
    assert filter_positives([-1, 2, -3, 4, 0]) == [2, 4]
    assert count_vowels("Hello World") == 3
    print("All tests passed!")

from abc import ABC, abstractmethod


# write the super class for comparison

class Comparison:
    def __init__(self):
        pass

    def compare(self, a, b):
        pass


# write the non-increasing function

class Descending(Comparison):
    def __init__(self):
        pass

    def compare(self, a, b):
        if a <= b:
            return True
        else:
            return False


# write the non-decreasing function

class Ascending(Comparison):
    def __init__(self):
        pass

    def compare(self, a, b):
        if a >= b:
            return True
        else:
            return False


# write the basesort class

class BaseSort(ABC):
    def __init__(self, strategy):
        self.strategy = strategy

    def pivot(self, array):
        pass

    # write the partition function to split the array

    def partition(self, array, pivot):
        left = []
        middle = []
        right = []

        for x in array:
            if x == pivot:
                middle.append(x)
            elif self.strategy.compare(pivot, x):
                left.append(x)
            else:
                right.append(x)

        return left, middle, right

    def real_sort(self, array):
        pass


# define the pivot as the first value of the array

class PivotFirst(BaseSort):
    def __init__(self, strategy):
        super().__init__(strategy)

    def pivot(self, array):
        pivot = array[0]
        return pivot

    # Recursion of real_sort
    def real_sort(self, array):
        if len(array) <= 1:
            return array
        else:
            pivotnum = self.pivot(array)
            left, middle, right = self.partition(array, pivotnum)
            sort_left = self.real_sort(left)
            sort_right = self.real_sort(right)

            return sort_left, middle, sort_right


# define the pivot as the last value of the array

class PivotLast(BaseSort):
    def __init__(self, strategy):
        super().__init__(strategy)

    def pivot(self, array):
        pivot = array[len(array) - 1]
        return pivot

    # Recursion of real_sort
    def real_sort(self, array):
        if len(array) <= 1:
            return array
        else:
            pivotnum = self.pivot(array)
            left, middle, right = self.partition(array, pivotnum)
            sort_left = self.real_sort(left)
            sort_right = self.real_sort(right)

            return sort_left, middle, sort_right

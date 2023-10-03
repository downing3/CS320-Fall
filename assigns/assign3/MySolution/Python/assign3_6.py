# Assign3-6: 30 points
# Please translate the datatype mylist (given in Assign2) into
# type classes (by following the example of fnlist in MyPython.py).
# Then please translate mylist_foreach and mylist_rforeach into Python
#
########################################################################

#type 'a mylist =
#  | MyNil
#  | MyCons of 'a * 'a mylist
#  | MySnoc of 'a mylist * 'a
#  | MyReverse of 'a mylist
#  | MyAppend2 of 'a mylist * 'a mylist

#EXAMPLE
#class fnlist:
#    ctag = -1
#    def get_ctag(self):
#        return self.ctag
#    def __iter__(self):
#        return fnlist_iter(self)
#    def __reversed__(self):
#        return fnlist_reverse(self)


class MyClass:
    def __init__(self, value):
        self.value = value

    @classmethod
    def empty_list(cls):
        return []

    @classmethod
    def append_to_front(cls, lst, value):
        lst.insert(0, value)  # inserting at the front
        return lst
    
    def add_to_end(self, lst, value):
        lst.append(value)
        return lst
    
    def reverse(self, lst):
        length = len(lst)
        output = []
        for x in lst[::-1]:
            output = self.add_to_end(output, x)
        return output
    
    def concatenate_two_lists(self, list1, list2):
        output = list1 + list2
        return output



    







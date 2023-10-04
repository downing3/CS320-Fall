
class mylist:
    ctag = -1
    def get_ctag(self):
        return self.ctag
    def __reversed__(self):
        return mylist_reverse(self)

##############################################

class mylist_nil(mylist):
    def __init__(self):
        self.ctag = 0
        #return None
# end-of-class(mylist_nil)


class mylist_cons(mylist):
    def __init__(self, head, tail):
        self.ctag = 1
        self.head = head
        self.tail = tail
        # return None
    def get_head(self):
        return self.head
    def get_tail(self):
        return self.tail
    
    def __iter__(self):
        current = self
        while isinstance(current, mylist_cons):
            yield current.head
            current = current.tail

#end-of-class (mylist_cons)


class mylist_snoc(mylist):
    def __init__(self, initial, last):
        self.ctag = 2
        self.initial = initial
        self.last = last

    def __iter__(self):
        current = self
        while isinstance(current, mylist_snoc):
            yield current.initial
            current = current.last
# end-of-class(mylist_snoc)


class mylist_reverse(mylist):
    def __init__(self, lst):
        self.ctag = 3
        self.lst = lst

    def __iter__(self):
        items = list(self.lst) 
        for item in reversed(items):
            yield item
# end-of-class(mylist_reverse)


class mylist_append2(mylist):
    def __init__(self, lst1, lst2):
        self.ctag = 4
        self.lst1 = lst1
        self.lst2 = lst2

    def __iter__(self):
        yield from self.lst1
        yield from self.lst2
# end-of-class(mylist_append2)

##############################################

def mylist_foreach(lst, func):
    for item in lst:
        func(item)

##############################################

def mylist_rforeach(lst, func):
    for item in reversed(lst):
        func(item)

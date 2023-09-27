def int1_foreach(n0, work_func):
    i0 = 0
    while(i0 < n0):
        work_func(i0)
        i0 = (i0 + 1)
    return None 

def string_merge(cs1, cs2):
    n1 = len(cs1)
    n2 = len(cs2)
    result = []

    def merge(i1, i2):
        if i1 < n1 and i2 < n2:
            c1 = cs1[i1]
            c2 = cs2[i2]
            if c1 <= c2:
                result.append(c1)
                return merge(i1 + 1, i2)
            else:
                result.append(c2)
                return merge(i1, i2 + 1)
        elif i1 < n1:
            def work(i):
                result.append(cs1[i1 + i])
            int1_foreach(n1 - i1, work)
        elif i2 < n2:
            def work(i):
                result.append(cs2[i2 + i])
            int1_foreach(n2 - i2, work)

    merge(0, 0)
    return ''.join(result)

################################################
#
# Assign4-6:
#
# HX-2023-10-06: 30 points (bonus)
#
# (*
# //
# Please implement the following function
# that enumerates all the pairs (i, j) of natural
# numbers satisfying $i <= j$; a pair (i1, j1) must
# be enumerated ahead of another pair (i2, j2) if the
# following condition holds:
#   i1*i1*i1 + j1*j1*j1 < i2*i2*i2 + j2*j2*j2
# //
# let
# theNatPairs_cubesum(): (int * int) stream = fn () =>
# //
# *)
#
# def theNatPairs_cubesum(): # please give your implementation
#
################################################

def theNatPairs_cubesum():
    limit = 1
    while True:
        pairs = [(i, j) for i in range(limit) for j in range(i, limit)]
        pairs.sort(key=lambda ij: ij[0]**3 + ij[1]**3)
        
        for i, j in pairs:
            yield i, j
            
        limit += 1


 




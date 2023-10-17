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
    i, j = 0, 0
    while True:
        i_cubed = i * i * i
        j_cubed = j * j * j
        yield i, j
        if i_cubed + j_cubed < i * i * i + j * j * j:
            j += 1
        else:
            i += 1
            j = i

